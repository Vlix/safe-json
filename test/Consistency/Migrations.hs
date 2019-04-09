{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Consistency.Migrations where


import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Data.SafeJSON
import Data.SafeJSON.Test (testRoundTrip, migrateRoundTrip, migrateReverseRoundTrip)


migrationConsistency :: TestTree
migrationConsistency = testGroup "Migration Consistency"
  [ roundTripTest
  , reverseRoundTripTest
  ]



shouldFail :: String -> String -> IO () -> TestTree
shouldFail s err io = testCase s $ do
    b <- tryIt `catch` success
    when b $ assertFailure err
  where tryIt = io >> return True
        success :: SomeException -> IO Bool
        success _ = return False


roundTripTest :: TestTree
roundTripTest = testCase "Round trip function test" $ do
    let oldType = OldType 1 "test"
    testRoundTrip oldType
    migrateRoundTrip @NewType oldType

data OldType = OldType Int Text
  deriving (Eq, Show)

instance FromJSON OldType where
  parseJSON = withObject "OldType" $ \o -> do
      i <- o .: "old_type_int"
      t <- o .: "old_type_text"
      return $ OldType i t

instance ToJSON OldType where
  toJSON (OldType i t) = object
      [ "old_type_int"  .= i
      , "old_type_text" .= t
      ]

instance SafeJSON OldType where
  version = noVersion
  kind = extended_base


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
  migrate (OldType i t) = NewType [i] $ if t == mempty then False else True

reverseRoundTripTest :: TestTree
reverseRoundTripTest = testCase "Reverse round trip function test" $ do
    let newType = NewType [1,2,3] False
    testRoundTrip newType
    migrateReverseRoundTrip @OldType newType

instance Migrate (Reverse OldType) where
  type MigrateFrom (Reverse OldType) = NewType
  migrate (NewType is b) = Reverse $ OldType i $ if b then "yes" else ""
    where i = case is of
                []    -> 0
                (x:_) -> x