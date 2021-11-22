{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module MigrationTests where


import Control.Applicative (Const)
import Control.Exception (try)
import Control.Monad (forM)
import Data.Aeson hiding (eitherDecodeFileStrict)
import Data.Aeson as A (decodeFileStrict)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KM (KeyMap)
#endif
import Data.Aeson.Types (parseEither)
import Data.DList (DList)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap as IntMap (IntMap, fromList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map as Map (Map, fromList)
import Data.Maybe (fromJust)
import Data.Monoid (Dual)
import Data.Proxy
import Data.Semigroup (First, Last, Max, Min)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID as UUID
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Aeson.Safe (eitherDecodeFileStrict)
import Data.SafeJSON
import Data.SafeJSON.Test
import Instances()
import Types


migrationTests :: TestTree
migrationTests = testGroup "Migrations"
    [ versionedPrimRoundTrip

    -- Migrating
    , testSafeParse "vNil-> v1" "Couldn't parse v1 from vNil" "noversion"  $ Version1 "12345"

    , testSafeParse "vNil-> v2" "Couldn't parse v2 from vNil" "noversion"  $ Version2 ["12345"]
    , testSafeParse "v1  -> v2" "Couldn't parse v2 from v1"   "version1"   $ Version2 ["success"]

    , testSafeParse "vNil-> v3" "Couldn't parse v3 from vNil" "noversion"  $ Version3 ["12345"] False
    , testSafeParse "v1  -> v3" "Couldn't parse v3 from v1"   "version1"   $ Version3 ["success"] False
    , testSafeParse "v2  -> v3" "Couldn't parse v3 from v2"   "version2"   $ Version3 ["success!","or","is","it?!"] False

    , testSafeParse "vNil-> v4" "Couldn't parse v4 from vNil" "noversion"  $ Version4 ["12345"] Nothing
    , testSafeParse "v1  -> v4" "Couldn't parse v4 from v1"   "version1"   $ Version4 ["success"] Nothing
    , testSafeParse "v2  -> v4" "Couldn't parse v4 from v2"   "version2"   $ Version4 ["success!","or","is","it?!"] Nothing
    , testSafeParse "v3  -> v4" "Couldn't parse v4 from v3"   "version3"   $ Version4 ["success!","or","is","it?!"] (Just $ posixSecondsToUTCTime 0)

    -- Reverse migrating
    , testSafeParse "v1  -> v0" "Couldn't parse v0 from v1"   "version1"   $ Version0 "success"
    , testSafeParse "v2  -> v1" "Couldn't parse v1 from v2"   "version2"   $ Version1 "success!, or, is, it?!"
    , testSafeParse "v4  -> v3" "Couldn't parse v3 from v4"   "version4"   $ Version3 ["success!","or","is","it?!"] True
    , testSafeParse "v4-2-> v3" "Couldn't parse v3 from v4-2" "version4-2" $ Version3 ["one","two","three"] False

    -- Should not migrate
    , testSafeNoParse @NoVersion "v2  -> vNil" "Shouldn't parse vNil from v2" "version2"   vNilError
    , testSafeNoParse @NoVersion "v3  -> vNil" "Shouldn't parse vNil from v3" "version3"   vNilError
    , testSafeNoParse @NoVersion "v4  -> vNil" "Shouldn't parse vNil from v4" "version4"   vNilError
    , testSafeNoParse @NoVersion "v4-2-> vNil" "Shouldn't parse vNil from v4" "version4-2" vNilError

    , testSafeNoParse @Version1 "v3  -> v1" "Shouldn't parse v1 from v3"   "version3"   $ noParserError "Version 3"
    , testSafeNoParse @Version1 "v4  -> v1" "Shouldn't parse v1 from v4"   "version4"   $ noParserError "Version 4"
    , testSafeNoParse @Version1 "v4-2-> v1" "Shouldn't parse v1 from v4-2" "version4-2" $ noParserError "Version 4"

    , testSafeNoParse @Version2 "v4  -> v2" "Shouldn't parse v2 from v4"   "version4"   $ noParserError "Version 4"
    , testSafeNoParse @Version2 "v4-2-> v2" "Shouldn't parse v2 from v4-2" "version4-2" $ noParserError "Version 4"

    -- Other
    , testSafeParse "simple versioning" "Couldn't parse simple data v1 from UUID"  "simpleversion" $ SimpleVersion1 UUID.nil ""
    , testSafeNoParse @BadVersion "bad simple" "Shouldn't parse from bad format" "badsimpleversion" $ EE "Error in $: malformed simple data (Version 8)"

    -- Container parsing
    , parseCollection @[]         @Version4 "[v4]"        allVersioned singleList
    , parseCollection @NonEmpty   @Version4 "NonEmpty v4" allVersioned singleList
    , parseCollection @DList      @Version4 "DList v4"    allVersioned singleList
    , parseCollection @V.Vector   @Version4 "V.Vector v4" allVersioned singleList
    , parseCollection @IntMap     @Version4 "IntMap v4"   allVersioned $ toJSON . IntMap.fromList  . zip [1..] . fmap snd
    , parseCollection @(Map Text) @Version4 "Map v4"      allVersioned objectList
    , parseCollection @(HashMap Text) @Version4 "HashMap v4" allVersioned objectList
#if MIN_VERSION_aeson(2,0,0)
    , parseCollection @KM.KeyMap  @Version4 "Aeson.KeyMap v4" allVersioned objectList
#endif
    , parseCollection @Set        @Version4 "Set v4"      allVersioned singleList
    , parseCollection @HashSet    @Version4 "HashSet v4"  allVersioned singleList
    , parseCollection @Seq        @Version4 "Seq v4"      allVersioned singleList

    , parseCollection @Maybe      @Version4 "Maybe v4"    allVersioned single
    , parseCollection @Identity   @Version4 "Identity v4" allVersioned single
    , parseCollection @(Const Version4) @Version4 "Const v4" allVersioned single
    , parseCollection @First      @Version4 "First v4"    allVersioned single
    , parseCollection @Last       @Version4 "Last  v4"    allVersioned single
    , parseCollection @Min        @Version4 "Min   v4"    allVersioned single
    , parseCollection @Max        @Version4 "Max   v4"    allVersioned single
    , parseCollection @Dual       @Version4 "Dual  v4"    allVersioned single

    , parseCollection @((,) Version4) @Version4 "(v4,v4)" allVersioned $ toJSON . mkTup2 . fmap snd
    , parseCollection @((,,) Version4 Version4) @Version4 "(v4,v4,v4)" allVersioned $ toJSON . mkTup3 . fmap snd
    , parseCollection @((,,,) Version4 Version4 Version4) @Version4 "(v4,v4,v4,v4)" allVersioned $ toJSON . mkTup4 . fmap snd
    , parseCollection @((,,,,) Version4 Version4 Version4 Version4) @Version4 "(v4,v4,v4,v4,v4)" allVersioned $ toJSON . mkTup5 . fmap snd

    , parseCollection     @[]     @Version3  "[v3]"       allVersioned                        singleList
    , parseCollection     @[]     @Version1  "[v1]"       ["noversion","version1","version2"] singleList
    , parseCollectionFail @[]     @NoVersion "[vNil] [FAIL]" allVersioned                     singleList
    , parseCollectionFail @[]     @Version1  "[v1] [FAIL]" allVersioned                       singleList
    , parseCollectionFail @[]     @Version2  "[v2] [FAIL]" allVersioned                       singleList
    ]

singleList :: ToJSON b => [(a,b)] -> Value
singleList = toJSON . fmap snd

objectList :: (Ord a, ToJSONKey a, ToJSON b) => [(a,b)] -> Value
objectList = toJSON . Map.fromList

single :: ToJSON b => [(a,b)] -> Value
single = toJSON . snd . mkSing

mkSing :: [a] -> a
mkSing (x:_) = x
mkTup2 :: [a] -> (a,a)
mkTup3 :: [a] -> (a,a,a)
mkTup4 :: [a] -> (a,a,a,a)
mkTup5 :: [a] -> (a,a,a,a,a)
mkTup2 (x:y:_) = (x,y)
mkTup3 (x:y:z:_) = (x,y,z)
mkTup4 (x:y:z:a:_) = (x,y,z,a)
mkTup5 (x:y:z:a:b:_) = (x,y,z,a,b)

----------------------------------------------------------
-- Versioned primitive round trip
----------------------------------------------------------

versionedPrimRoundTrip :: TestTree
versionedPrimRoundTrip = testRoundTripProp @VersionedPrim "Round trip (VersionedPrim)"

newtype VersionedPrim = VersionedPrim { unVersionedPrim :: Text } deriving (Eq, Show)

instance FromJSON  VersionedPrim where parseJSON = withText "VersionedPrim" $ pure . VersionedPrim
instance ToJSON    VersionedPrim where toJSON    = String . unVersionedPrim
instance SafeJSON  VersionedPrim where version   = 2
instance Arbitrary VersionedPrim where arbitrary = VersionedPrim . pack <$> arbitrary


----------------------------------------------------------
-- Migration through versions
----------------------------------------------------------

newtype ExpectedError = EE String deriving (Eq, Show)

vNilError :: ExpectedError
#if MIN_VERSION_aeson(1,4,6)
vNilError = EE "Error in $: key \"int\" not found"
#else
vNilError = EE "Error in $: key \"int\" not present"
#endif

noParserError :: String -> ExpectedError
noParserError = EE . mappend "Error in $: Cannot find parser associated with: "

jsonPath :: FilePath
jsonPath = "test/json/"

testSafeParse :: (Eq a, Show a, SafeJSON a) => String -> String -> FilePath -> a -> TestTree
testSafeParse s a fp x = testCase s $ do
    eVal <- eitherDecodeFileStrict $ jsonPath ++ fp ++ ".json"
    case eVal of
      Left err -> assertFailure err
      Right val -> assertEqual a x val

testSafeNoParse :: forall a. SafeJSON a => String -> String -> FilePath -> ExpectedError -> TestTree
testSafeNoParse s a fp (EE ee) = testCase (s ++ " [FAIL]") $ do
    eVal <- eitherDecodeFileStrict $ jsonPath ++ fp ++ ".json"
    case eVal :: Either String a of
      Left err -> assertEqual a ee err
      Right{}  -> assertFailure $ fp ++ ": should not have parsed a " ++ typeName (Proxy @a)


----------------------------------------------------------
-- Migration through collections
----------------------------------------------------------

allVersioned :: [String]
allVersioned = ["noversion","version1","version2","version3","version4"]

parseCollection' :: forall f a.
                    ( SafeJSON a
                    , SafeJSON (f a)
                    )
                 => (IO () -> IO ()) -> String -> [String] -> ([(Text,Value)] -> Value) -> TestTree
parseCollection' io s files f = testCase s $ io $ do
    let fp x = jsonPath ++ x ++ ".json"
        addFile x mA = (,) (pack x) <$> mA
    mLs <- forM files $ \file -> addFile file <$> A.decodeFileStrict (fp file)
    let ls = fromJust <$> mLs :: [(Text, Value)]
        eVal = parseEither safeFromJSON $ f ls :: Either String (f a)
    case eVal of
      Right{} -> return ()
      Left err -> assertFailure $ mconcat
          ["Could not parse collection of different versions (", typeName (Proxy :: Proxy a), "): ", err]

parseCollection :: forall f a.
                   ( SafeJSON a
                   , SafeJSON (f a)
                   )
                => String -> [String] -> ([(Text,Value)] -> Value) -> TestTree
parseCollection = parseCollection' @f @a id

parseCollectionFail :: forall f a.
                       ( SafeJSON a
                       , SafeJSON (f a)
                       )
                    => String -> [String] -> ([(Text,Value)] -> Value) -> TestTree
parseCollectionFail = parseCollection' @f @a go
  where go :: IO () -> IO ()
        go io = try @HUnitFailure io >>= \case
            Left e -> return ()
            Right{} -> assertFailure "Should have failed"
