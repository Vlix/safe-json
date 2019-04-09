{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module MigrationTests where


import Data.Aeson
import Data.Text (Text, pack)
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.SafeJSON
import Data.SafeJSON.Test
-- import Types


migrationTests :: TestTree
migrationTests = testGroup "Migrations"
    [ versionedPrimRoundTrip
    -- , migrateNoVersion
    -- , migrateVersion1
    ]


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
