{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module ConsistencyTests where


import Data.Aeson
import Test.Tasty

import Data.SafeJSON


consistencyTests :: TestTree
consistencyTests = testGroup "Consistency"
  []

data OldType = OldType deriving (Eq, Show)

data NewType = NewType

instance SafeJSON OldType where
  version = 0
  kind = extended_extension

instance SafeJSON NewType where
  version = 1
  kind = extended_extension

instance ToJSON OldType where
  toJSON _ = Null

instance ToJSON NewType where
  toJSON _ = Null

instance FromJSON OldType where
  parseJSON Null = pure OldType
  parseJSON _ = fail "uhhh wat"

instance FromJSON NewType where
  parseJSON Null = pure NewType
  parseJSON _ = fail "uhhh wat"

instance Migrate OldType where
  type MigrateFrom OldType = NewType
  migrate = const OldType

instance Migrate NewType where
  type MigrateFrom NewType = OldType
  migrate = const NewType

instance Migrate (Reverse OldType) where
  type MigrateFrom (Reverse OldType) = NewType
  migrate = const $ Reverse OldType

instance Migrate (Reverse NewType) where
  type MigrateFrom (Reverse NewType) = OldType
  migrate = const $ Reverse NewType


data BadJSON = BadJSON Int deriving (Eq, Show)

instance FromJSON BadJSON where
  parseJSON = withText "BadJSON" $ \case
    "bad" -> pure $ BadJSON 1
    _ -> fail "wat"

instance ToJSON BadJSON where
  toJSON (BadJSON 2) = String "bad"
  toJSON _ = String "wat"

instance SafeJSON BadJSON where
  version = noVersion
