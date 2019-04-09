{-# LANGUAGE TypeApplications #-}
module MigrationTests where


import Data.Aeson
import Data.Text (Text, pack)
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.SafeJSON
import Data.SafeJSON.Test


migrationTests :: TestTree
migrationTests = testGroup "Migrations"
    [ versionedPrimRoundTrip
    ]


versionedPrimRoundTrip :: TestTree
versionedPrimRoundTrip = testRoundTripProp @VersionedPrim "Round trip (VersionedPrim)"


newtype VersionedPrim = VersionedPrim Text
  deriving (Eq, Show)

instance FromJSON VersionedPrim where
  parseJSON = withText "VersionedPrim" $ pure . VersionedPrim

instance ToJSON VersionedPrim where
  toJSON (VersionedPrim t) = String t

instance SafeJSON VersionedPrim where
  version = 2

instance Arbitrary VersionedPrim where
  arbitrary = VersionedPrim . pack <$> arbitrary
