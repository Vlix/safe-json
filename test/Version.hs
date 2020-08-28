{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Version where


import Control.Exception (handle)
import Control.Monad (when)
import qualified Data.Aeson as A
import Data.Aeson.Safe
import Data.Text as T
import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty
import Test.Tasty.QuickCheck as Tasty

import Data.SafeJSON.Test


versionFuncTests :: TestTree
versionFuncTests = testGroup "Version functions"
  [ consistency
  , setTest
  , removeTest
  ]


consistency :: TestTree
consistency = testGroup "Consistency/Round trips"
    [ testCase "C: TestObject" $ testConsistency @(TestObject ())
    , testCase "C: TestArray" $ testConsistency @TestArray
    , testRoundTripProp @(TestObject ()) "R: TestObject"
    , testRoundTripProp @TestArray "R: TestArray"
    ]

setTest :: TestTree
setTest = testGroup "Set version"
    [ parseSetVersion "SimpleVersion" $ TestArray [0,1,2,3]
    , parseSetVersion "WithVersion1" $ TestObject $ String "testing1"
    , let shouldBe = TestObject $ TestObject $ String "testing2"
          isActually = safeToJSON $ TestObject $ go "testing2"
      in parseSetVersionFail "WithVersion2" shouldBe isActually
    , let shouldBe = TestObject [TestObject $ String "testing3", TestObject $ String "testing4"]
          isActually = safeToJSON $ TestObject $ A.toJSON [go "testing3", go "testing4"]
      in parseSetVersionFail "WithVersion3" shouldBe isActually
    , parseAnd "reSetVersion" $ \(to2, val0) -> do
        let obj2 = TestObject2 $ String "testing1"
        assertEqual "Should equal" obj2 to2
        assertBool "Should not equal" $ safeToJSON to2 /= val0
        assertEqual "Version override failed" (safeToJSON obj2) (setVersion @(TestObject2 Value) val0)
    , parseAnd "reSetVersionSimple" $ \(ta3, val1) -> do
        let arr3 = TestArray3 [0,1,2,3]
        assertEqual "Should equal" arr3 ta3
        assertBool "Should not equal" $ safeToJSON ta3 /= val1
        assertEqual "Version override failed" (safeToJSON arr3) (setVersion @TestArray3 val1)
    ]
  where go t = object [ "test" .= String t ]

removeTest :: TestTree
removeTest = testGroup "Remove version"
    [ parseRemoveVersion "SimpleVersion" $ TestArray [0,1,2,3]
    , parseRemoveVersion "WithVersion1" $ TestObject $ String "testing1"
    , parseRemoveVersion "WithVersion2" $ TestObject $ TestObject $ String "testing2"
    , parseRemoveVersion "WithVersion3" $ TestObject [TestObject $ String "testing3", TestObject $ String "testing4"]
    ]


-- | Given a field in the "version.json" object, parses as
-- the given type, but hardsets version before doing so.
parseSetVersion :: forall a. SafeJSON a => Text -> a -> TestTree
parseSetVersion t val = parseAnd t go
  where safeVal = safeToJSON val
        go (with,without) = do
          assertEqual "With: as regular" safeVal with
          assertEqual "Without: after version added" safeVal $ setVersion @a without

-- | Like 'parseSetVersion', but expects to fail on the second.
parseSetVersionFail :: forall a. SafeJSON a => Text -> a -> Value -> TestTree
parseSetVersionFail t val actual = parseAnd t go
  where safeVal = safeToJSON val
        err HUnitFailure{} = return True
        go (with,without) = do
          assertEqual "With: as regular" safeVal with
          failed <- handle err $ do
              assertEqual "Without: after version added" safeVal $ setVersion @a without
              return False
          when (not failed) $ assertFailure "Expected to fail"
          assertEqual "Unexpected behaviour" actual $ setVersion @a without

-- | Given a field in the "version.json" object, tries to
-- compare the plain JSON with the (removeVersion . safeToJSON)
-- 'Value' of the provided type.
parseRemoveVersion :: forall a. SafeJSON a => Text -> a -> TestTree
parseRemoveVersion t val = parseAnd t go
  where safeVal = safeToJSON val
        go (with,without) = do
          assertEqual "With: as regular" safeVal with
          assertEqual "Without: after versions removed" (removeVersion safeVal) without

parseAnd :: SafeJSON a => Text -> ((a,Value) -> IO ()) -> TestTree
parseAnd t f = testCase (T.unpack t) $
    A.decodeFileStrict ("test/json/setremoveversion.json")
      >>= maybe (assertFailure "couldn't read file")
                (either fail f . parseEither go)
  where go = A.withObject "test" $ \o -> do
                o .: t >>= \o2 -> (,) <$> (o2 .: "with" >>= safeFromJSON) <*> o2 .: "without"

data TestObject a = TestObject {
  testObject :: a
} deriving (Eq, Show)

instance Arbitrary a => Arbitrary (TestObject a) where
  arbitrary = TestObject <$> arbitrary

instance SafeJSON a => SafeJSON (TestObject a) where
  safeFrom = contain . go
    where go = withObject "TestObject" $ \o ->
                  TestObject <$> (o .: "test" >>= safeFromJSON)
  safeTo to = contain $ object [ "test" .= safeToJSON (testObject to) ]
  typeName = typeName1


newtype TestArray = TestArray [Int]
  deriving (Eq, Show)

instance Arbitrary TestArray where
  arbitrary = TestArray <$> arbitrary

instance SafeJSON TestArray where
  safeFrom = contain . fmap TestArray . safeFromJSON
  safeTo (TestArray is) = contain $ safeToJSON is
  version = 1


------------ USED FOR TESTING VERSION OVERRIDE -----------
------------ USED FOR TESTING VERSION OVERRIDE -----------
------------ USED FOR TESTING VERSION OVERRIDE -----------

data TestObject2 a = TestObject2 {
  testObject2 :: a
} deriving (Eq, Show)

instance Arbitrary a => Arbitrary (TestObject2 a) where
  arbitrary = TestObject2 <$> arbitrary

instance SafeJSON a => SafeJSON (TestObject2 a) where
  safeFrom = contain . go
    where go = withObject "TestObject2" $ \o ->
                  TestObject2 <$> (o .: "test" >>= safeFromJSON)
  safeTo to = contain $ object [ "test" .= safeToJSON (testObject2 to) ]
  typeName = typeName1
  version = 2

newtype TestArray3 = TestArray3 [Int]
  deriving (Eq, Show)

instance Arbitrary TestArray3 where
  arbitrary = TestArray3 <$> arbitrary

instance SafeJSON TestArray3 where
  safeFrom = contain . fmap TestArray3 . safeFromJSON
  safeTo (TestArray3 is) = contain $ safeToJSON is
  version = 3
