{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module PrimitiveTests where


import Control.Applicative (Const)
import Data.Aeson (DotNetTime, Value, (.:))
import qualified Data.Aeson as A
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
#endif
import Data.Aeson.Types (parseEither)
import Data.DList (DList)
import Data.Fixed (E12, Fixed)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Monoid (Dual)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Scientific (Scientific)
import Data.Semigroup (First, Last, Max, Min)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text as T (Text, unpack)
import Data.Text.Lazy as LT (Text)
import Data.Time
import Data.Tree (Tree)
import Data.UUID.Types (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Version as DV (Version)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CTime)
import Numeric.Natural (Natural)

import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)

import Data.SafeJSON
import Instances()


primitiveTests :: TestTree
primitiveTests = testGroup "Primitives"
  [ regularParsing
  , toJSONEquivalence
  , fromJSONEquivalence
  ]


parseValueAnd :: forall a. SafeJSON a => String -> (a -> IO ()) -> TestTree
parseValueAnd s f = testCase s $ do
    mVal <- A.decodeFileStrict "test/json/primitives.json"
    maybe
      (assertFailure "couldn't read file")
      (either fail f . parseEither go)
      mVal
  where go = A.withObject "test" $ \o -> do
                o .: fromString s >>= safeFromJSON

parseValue :: forall a. SafeJSON a => String -> TestTree
parseValue t = parseValueAnd t (const $ return () :: a -> IO ())

fromJSONTest :: forall a. (A.FromJSON a, SafeJSON a, Eq a, Show a)
             => String -> TestTree
fromJSONTest t = parseValueAnd t $ \val -> do
    let a = (parseEither A.parseJSON val :: Either String a)
        b = parseEither safeFromJSON val
    assertEqual "SafeJSON not equivalent to FromJSON" a b

toJSONTest :: forall a. (A.ToJSON a, SafeJSON a, Arbitrary a, Show a)
           => String -> TestTree
toJSONTest s = testProperty s $ \a -> A.toJSON a == safeToJSON (a :: a)


regularParsing :: TestTree
regularParsing = testGroup "Parsing from JSON"
  [ parseValue @Bool       "Bool"
  , parseValue @Ordering   "Ordering"
  , parseValue @()         "Unit"
  , parseValue @Char       "Char"
  , parseValue @Float      "Float"
  , parseValue @Double     "Double"
  , parseValue @Int        "Int"
  , parseValue @Natural    "Natural"
  , parseValue @Integer    "Integer"
  , parseValue @Int8       "Int8"
  , parseValue @Int16      "Int16"
  , parseValue @Int32      "Int32"
  , parseValue @Int64      "Int64"
  , parseValue @Word       "Word"
  , parseValue @Word8      "Word8"
  , parseValue @Word16     "Word16"
  , parseValue @Word32     "Word32"
  , parseValue @Word64     "Word64"
  , parseValue @T.Text     "T.Text"
  , parseValue @LT.Text    "LT.Text"
#if MIN_VERSION_aeson(2,0,0)
  , parseValue @K.Key      "Aeson.Key"
#endif
  , parseValue @DV.Version "Version"
  , parseValue @Scientific "Scientific"
  , parseValue @IntSet     "IntSet"
  , parseValue @UUID       "UUID"
  , parseValue @CTime      "CTime"
  , parseValue @ZonedTime  "ZonedTime"
  , parseValue @LocalTime  "LocalTime"
  , parseValue @TimeOfDay  "TimeOfDay"
  , parseValue @UTCTime    "UTCTime"
  , parseValue @NominalDiffTime "NominalDiffTime"
  , parseValue @DiffTime   "DiffTime"
  , parseValue @Day        "Day"
  , parseValue @DotNetTime "DotNetTime"
  , parseValue @Value      "Value"

  , parseValue @(Ratio Int) "Ratio"
  , parseValue @(Fixed E12) "Fixed"
  , parseValue @(Proxy ()) "Proxy"

  , parseValue @(Identity T.Text) "Identity"
  , parseValue @(First T.Text)    "First"
  , parseValue @(Last T.Text)     "Last"
  , parseValue @(Min T.Text)      "Min"
  , parseValue @(Max T.Text)      "Max"
  , parseValue @(Dual T.Text)     "Dual"

  , parseValue @([Int])        "[]"
  , parseValue @(IntMap Bool)  "IntMap"
  , parseValue @(NonEmpty Int) "NonEmpty"
  , parseValue @(Seq T.Text)   "Seq"
  , parseValue @(Tree T.Text)  "Tree"

  , parseValue @(Const T.Text ())    "Const"
  , parseValue @(Maybe T.Text)       "Maybe"
  , parseValue @(Maybe T.Text)       "Maybe2"
  , parseValue @(Either T.Text Bool) "Either"
  , parseValue @(Either T.Text Bool) "Either2"

  , parseValue @(DList Int)     "DList"
  , parseValue @(V.Vector Int)  "V.Vector"
  , parseValue @(VS.Vector Int) "VS.Vector"
  , parseValue @(VP.Vector Int) "VP.Vector"
  , parseValue @(VU.Vector Int) "VU.Vector"

  , parseValue @(Set Int)            "Set"
  , parseValue @(Map T.Text Int)     "Map"
  , parseValue @(HashSet Int)        "HashSet"
  , parseValue @(HashMap T.Text Int) "HashMap"
#if MIN_VERSION_aeson(2,0,0)
  , parseValue @(KM.KeyMap T.Text)   "Aeson.KeyMap"
#endif

  , parseValue @(Int, Bool)                        "Tuple2"
  , parseValue @(Int, Bool, T.Text)                "Tuple3"
  , parseValue @(Int, Bool, T.Text, [Int])         "Tuple4"
  , parseValue @(Int, Bool, T.Text, [Int], Double) "Tuple5"
  ]


--------------------------------

toJSONEquivalence :: TestTree
toJSONEquivalence = testGroup "safeToJSON === toJSON"
  [ toJSONTest @Bool         "Bool"
  , toJSONTest @Ordering     "Ordering"
  , toJSONTest @()           "()"
  , toJSONTest @Char         "Char"
  , toJSONTest @Float        "Float"
  , toJSONTest @Double       "Double"
  , toJSONTest @Int          "Int"
  , toJSONTest @Natural      "Natural"
  , toJSONTest @Integer      "Integer"
  , toJSONTest @Int8         "Int8"
  , toJSONTest @Int16        "Int16"
  , toJSONTest @Int32        "Int32"
  , toJSONTest @Int64        "Int64"
  , toJSONTest @Word         "Word"
  , toJSONTest @Word8        "Word8"
  , toJSONTest @Word16       "Word16"
  , toJSONTest @Word32       "Word32"
  , toJSONTest @Word64       "Word64"
  , toJSONTest @T.Text       "T.Text"
  , toJSONTest @LT.Text      "LT.Text"
#if MIN_VERSION_aeson(2,0,0)
  , toJSONTest @K.Key        "Aeson.Key"
#endif
  , toJSONTest @DV.Version   "DV.Version"
  , toJSONTest @Scientific   "Scientific"
  , toJSONTest @IntSet       "IntSet"
  , toJSONTest @UUID         "UUID"
  , toJSONTest @CTime        "CTime"
  , toJSONTest @ZonedTime    "ZonedTime"
  , toJSONTest @LocalTime    "LocalTime"
  , toJSONTest @TimeOfDay    "TimeOfDay"
  , toJSONTest @UTCTime      "UTCTime"
  , toJSONTest @NominalDiffTime "NominalDiffTime"
  , toJSONTest @DiffTime     "DiffTime"
  , toJSONTest @Day          "Day"
  , toJSONTest @DotNetTime   "DotNetTime"
  , toJSONTest @Value        "Value"

  , toJSONTest @(Ratio Int)                "Ratio"
  , toJSONTest @(Fixed E12)                "Fixed"
  , toJSONTest @(Proxy ())                 "Proxy"
  , toJSONTest @(Identity T.Text)          "Identity"
  , toJSONTest @(First T.Text)             "First"
  , toJSONTest @(Last T.Text)              "Last"
  , toJSONTest @(Min T.Text)               "Min"
  , toJSONTest @(Max T.Text)               "Max"
  , toJSONTest @(Dual T.Text)              "Dual"
  , toJSONTest @([Int])                    "[]"
  , toJSONTest @(IntMap Bool)              "IntMap"
  , toJSONTest @(NonEmpty Int)             "NonEmpty"
  , toJSONTest @(Seq T.Text)               "Seq"
  , toJSONTest @(Tree T.Text)              "Tree"
  , toJSONTest @(Const T.Text ())          "Const"
  , toJSONTest @(Maybe T.Text)             "Maybe"
  , toJSONTest @(Maybe T.Text)             "Maybe2"
  , toJSONTest @(Either T.Text Bool)       "Either"
  , toJSONTest @(Either T.Text Bool)       "Either2"
  , toJSONTest @(DList Int)                "DList"
  , toJSONTest @(V.Vector Int)             "V.Vector"
  , toJSONTest @(VS.Vector Int)            "VS.Vector"
  , toJSONTest @(VP.Vector Int)            "VP.Vector"
  , toJSONTest @(VU.Vector Int)            "VU.Vector"
  , toJSONTest @(Set Int)                  "Set"
  , toJSONTest @(Map T.Text Int)           "Map"
  , toJSONTest @(HashSet Int)              "HashSet"
  , toJSONTest @(HashMap T.Text Int)       "HashMap"
#if MIN_VERSION_aeson(2,0,0)
  , toJSONTest @(KM.KeyMap T.Text)         "Aeson.KeyMap"
#endif
  , toJSONTest @(Int, Bool)                "Tuple2"
  , toJSONTest @(Int, Bool, T.Text)        "Tuple3"
  , toJSONTest @(Int, Bool, T.Text, [Int]) "Tuple4"
  , toJSONTest @(Int, Bool, T.Text, [Int], Double) "Tuple5"
  ]

fromJSONEquivalence :: TestTree
fromJSONEquivalence = testGroup "safeFromJSON === fromJSON"
  [ fromJSONTest @Bool         "Bool"
  , fromJSONTest @Ordering     "Ordering"
  , fromJSONTest @()           "Unit"
  , fromJSONTest @Char         "Char"
  , fromJSONTest @Float        "Float"
  , fromJSONTest @Double       "Double"
  , fromJSONTest @Int          "Int"
  , fromJSONTest @Natural      "Natural"
  , fromJSONTest @Integer      "Integer"
  , fromJSONTest @Int8         "Int8"
  , fromJSONTest @Int16        "Int16"
  , fromJSONTest @Int32        "Int32"
  , fromJSONTest @Int64        "Int64"
  , fromJSONTest @Word         "Word"
  , fromJSONTest @Word8        "Word8"
  , fromJSONTest @Word16       "Word16"
  , fromJSONTest @Word32       "Word32"
  , fromJSONTest @Word64       "Word64"
  , fromJSONTest @T.Text       "T.Text"
  , fromJSONTest @LT.Text      "LT.Text"
#if MIN_VERSION_aeson(2,0,0)
  , fromJSONTest @K.Key        "Aeson.Key"
#endif
  , fromJSONTest @DV.Version   "Version"
  , fromJSONTest @Scientific   "Scientific"
  , fromJSONTest @IntSet       "IntSet"
  , fromJSONTest @UUID         "UUID"
  , fromJSONTest @CTime        "CTime"
  -- , fromJSONTest @ZonedTime    "ZonedTime" -- No Eq instance
  , fromJSONTest @LocalTime    "LocalTime"
  , fromJSONTest @TimeOfDay    "TimeOfDay"
  , fromJSONTest @UTCTime      "UTCTime"
  , fromJSONTest @NominalDiffTime "NominalDiffTime"
  , fromJSONTest @DiffTime     "DiffTime"
  , fromJSONTest @Day          "Day"
  , fromJSONTest @DotNetTime   "DotNetTime"
  , fromJSONTest @Value        "Value"

  , fromJSONTest @(Ratio Int)                "Ratio"
  , fromJSONTest @(Fixed E12)                "Fixed"
  , fromJSONTest @(Proxy ())                 "Proxy"
  , fromJSONTest @(Identity T.Text)          "Identity"
  , fromJSONTest @(First T.Text)             "First"
  , fromJSONTest @(Last T.Text)              "Last"
  , fromJSONTest @(Min T.Text)               "Min"
  , fromJSONTest @(Max T.Text)               "Max"
  , fromJSONTest @(Dual T.Text)              "Dual"
  , fromJSONTest @([Int])                    "[]"
  , fromJSONTest @(IntMap Bool)              "IntMap"
  , fromJSONTest @(NonEmpty Int)             "NonEmpty"
  , fromJSONTest @(Seq T.Text)               "Seq"
  , fromJSONTest @(Tree T.Text)              "Tree"
  , fromJSONTest @(Const T.Text ())          "Const"
  , fromJSONTest @(Maybe T.Text)             "Maybe"
  , fromJSONTest @(Maybe T.Text)             "Maybe2"
  , fromJSONTest @(Either T.Text Bool)       "Either"
  , fromJSONTest @(Either T.Text Bool)       "Either2"
  , fromJSONTest @(DList Int)                "DList"
  , fromJSONTest @(V.Vector Int)             "V.Vector"
  , fromJSONTest @(VS.Vector Int)            "VS.Vector"
  , fromJSONTest @(VP.Vector Int)            "VP.Vector"
  , fromJSONTest @(VU.Vector Int)            "VU.Vector"
  , fromJSONTest @(Set Int)                  "Set"
  , fromJSONTest @(Map T.Text Int)           "Map"
  , fromJSONTest @(HashSet Int)              "HashSet"
  , fromJSONTest @(HashMap T.Text Int)       "HashMap"
#if MIN_VERSION_aeson(2,0,1)
  , fromJSONTest @(KM.KeyMap T.Text)         "Aeson.KeyMap"
#endif
  , fromJSONTest @(Int, Bool)                "Tuple2"
  , fromJSONTest @(Int, Bool, T.Text)        "Tuple3"
  , fromJSONTest @(Int, Bool, T.Text, [Int]) "Tuple4"
  , fromJSONTest @(Int, Bool, T.Text, [Int], Double) "Tuple5"
  ]
