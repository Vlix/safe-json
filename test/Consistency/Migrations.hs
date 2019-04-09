{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Consistency.Migrations where


import Data.Aeson
import Data.Text (Text, pack)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.SafeJSON
import Data.SafeJSON.Test


-- 'OldType' and 'NewType' should be well-defined for these
-- tests to succeed.
migrationConsistency :: TestTree
migrationConsistency = testGroup "Migration Consistency"
  [ oldNewTypeChecks
  , roundTripTest
  , reverseRoundTripTest
  , roundTripTestProp
  , reverseRoundTripTestProp
  ]


oldNewTypeChecks :: TestTree
oldNewTypeChecks = testGroup "Consistent Old-/NewType"
    [ testCase "OldType is consistent" $ testConsistency' @OldType
    , testCase "NewType is consistent" $ testConsistency' @NewType
    , testRoundTripProp @OldType "Round trip (OldType)"
    , testRoundTripProp @NewType "Round trip (NewType)"
    ]

roundTripTest :: TestTree
roundTripTest = testCase "Round trip function test" $ do
    let oldType = OldType "test" 1
    testRoundTrip oldType
    migrateRoundTrip @NewType oldType

reverseRoundTripTest :: TestTree
reverseRoundTripTest = testCase "Reverse round trip function test" $ do
    let newType = NewType [1,2,3] False
    testRoundTrip newType
    migrateReverseRoundTrip @OldType newType

roundTripTestProp :: TestTree
roundTripTestProp = migrateRoundTripProp @NewType @OldType "Round trip property function test"

reverseRoundTripTestProp :: TestTree
reverseRoundTripTestProp = migrateReverseRoundTripProp @OldType @NewType "Reverse round trip property function test"


----------------------------------------------------------
-- Well-defined types
----------------------------------------------------------

data OldType = OldType Text Int
  deriving (Eq, Show)

instance FromJSON OldType where
  parseJSON = withObject "OldType" $ \o -> do
      i <- o .: "old_type_int"
      t <- o .: "old_type_text"
      return $ OldType t i

instance ToJSON OldType where
  toJSON (OldType t i) = object
      [ "old_type_int"  .= i
      , "old_type_text" .= t
      ]

instance SafeJSON OldType where
  version = noVersion
  kind = extended_base

instance Arbitrary OldType where
  arbitrary = OldType . pack <$> arbitrary <*> arbitrary


data NewType = NewType {
  newInts :: [Int],
  hasText :: Bool
} deriving (Eq, Show)

instance FromJSON NewType where
  parseJSON = withObject "NewType" $ \o -> do
      newInts <- o .: "new_type_ints"
      hasText <- o .: "new_type_bool"
      return NewType{..}

instance ToJSON NewType where
  toJSON (NewType is b) = object
      [ "new_type_ints" .= is
      , "new_type_bool" .= b
      ]

instance SafeJSON NewType where
  version = 1
  kind = extension

instance Migrate NewType where
  type MigrateFrom NewType = OldType
  migrate (OldType t i) = NewType [i] $ if t == mempty then False else True

instance Arbitrary NewType where
  arbitrary = NewType <$> arbitrary <*> arbitrary


instance Migrate (Reverse OldType) where
  type MigrateFrom (Reverse OldType) = NewType
  migrate (NewType is b) = Reverse $ OldType t i
    where i = case is of
                []    -> 0
                (x:_) -> x
          t = if b then "yes" else ""
