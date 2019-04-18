{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Types where


import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Hashable (Hashable(..))
import Data.Maybe (isJust)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid ((<>))
#endif
import Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID as UUID

import Data.SafeJSON


----------------------------------------------------------
-- Versioned chain
----------------------------------------------------------

checkType :: String -> Object -> Parser ()
checkType t o = do
    typ <- o .: "type"
    when (typ /= t) $ fail $ "wrong type (" ++ t ++ ")"

newtype NoVersion = NoVersion Int deriving (Eq, Show)
instance SafeJSON NoVersion where version = noVersion

instance FromJSON NoVersion where
  parseJSON = withObject "NoVersion" $ \o -> do
      checkType "test" o
      i <- o .: "int"
      return $ NoVersion i

instance ToJSON NoVersion where
  toJSON (NoVersion i) = object
      [ "type" .= String "test"
      , "int"  .= i
      ]


newtype Version0 = Version0 Text deriving (Eq, Show)
instance SafeJSON Version0 where kind = extended_base

instance FromJSON Version0 where
  parseJSON = withObject "Version0" $ \o -> do
      checkType "test" o
      t <- o .: "text"
      return $ Version0 t

instance ToJSON Version0 where
  toJSON (Version0 t) = object
      [ "type" .= String "test"
      , "text" .= t
      ]

instance Migrate (Reverse Version0) where
  type MigrateFrom (Reverse Version0) = Version1
  migrate (Version1 t) = Reverse $ Version0 t


newtype Version1 = Version1 Text deriving (Eq, Show)
instance SafeJSON Version1 where version = 1; kind = extended_extension

instance FromJSON Version1 where
  parseJSON = withObject "Version1" $ \o -> do
      checkType "test" o
      t <- o .: "text"
      return $ Version1 t

instance ToJSON Version1 where
  toJSON (Version1 t) = object
      [ "type" .= String "test"
      , "text" .= t
      ]

instance Migrate Version1 where
  type MigrateFrom Version1 = NoVersion
  migrate (NoVersion i) = Version1 . pack . show $ i

instance Migrate (Reverse Version1) where
  type MigrateFrom (Reverse Version1) = Version2
  migrate (Version2 ts) = Reverse . Version1 $ intercalate ", " ts


newtype Version2 = Version2 [Text] deriving (Eq, Show)
instance SafeJSON Version2 where version = 2; kind = extension

instance FromJSON Version2 where
  parseJSON = withObject "Version2" $ \o -> do
      checkType "test" o
      t <- o .: "texts"
      return $ Version2 t

instance ToJSON Version2 where
  toJSON (Version2 ts) = object
      [ "type"  .= String "test"
      , "texts" .= ts
      ]

instance Migrate Version2 where
  type MigrateFrom Version2 = Version1
  migrate (Version1 t) = Version2 [t]


data Version3 = Version3 {
  v3texts :: [Text],
  v3Closed :: Bool
} deriving (Eq, Show)

instance SafeJSON Version3 where version = 3; kind = extended_extension

instance FromJSON Version3 where
  parseJSON = withObject "Version3" $ \o -> do
      checkType "test" o
      ts <- o .: "texts"
      b  <- o .: "closed"
      return $ Version3 ts b

instance ToJSON Version3 where
  toJSON (Version3 ts b) = object
      [ "type"   .= String "test"
      , "texts"  .= ts
      , "closed" .= b
      ]

instance Migrate Version3 where
  type MigrateFrom Version3 = Version2
  migrate (Version2 ts) = Version3 ts False

instance Migrate (Reverse Version3) where
  type MigrateFrom (Reverse Version3) = Version4
  migrate (Version4 ts mTime) = Reverse $ Version3 ts $ isJust mTime


data Version4 = Version4 {
  v4texts :: [Text],
  v4TimeClosed :: Maybe UTCTime
} deriving (Eq, Show, Ord)

instance SafeJSON Version4 where version = 4; kind = extension

instance FromJSON Version4 where
  parseJSON = withObject "Version4" $ \o -> do
      checkType "test" o
      ts <- o .:  "texts"
      c  <- o .:? "closed"
      return $ Version4 ts c

instance ToJSON Version4 where
  toJSON (Version4 ts c) = object $
      [ "type"   .= String "test"
      , "texts"  .= ts
      ] ++  ["closed" .= x | Just x <- [c]]

instance Migrate Version4 where
  type MigrateFrom Version4 = Version3
  migrate (Version3 ts b) = Version4 ts time
    where time = if b then Just (posixSecondsToUTCTime 0) else Nothing

----------------------------------------------------------
-- Simple Version
----------------------------------------------------------

data SimpleVersion1 = SimpleVersion1 {
  s1UUID :: UUID,
  s1Name :: Text
} deriving (Eq, Show)

instance FromJSON SimpleVersion1 where
  parseJSON = withText "SimpleVersion1" $ \t ->
      let (ident,name) = T.span (/= ':') t
      in case UUID.fromText ident of
          Nothing -> fail "non-UUID prefix"
          Just uuid -> pure $ SimpleVersion1 uuid $ T.drop 1 name

instance ToJSON SimpleVersion1 where
  toJSON (SimpleVersion1 uuid name) = String $ UUID.toText uuid <> ":" <> name

instance SafeJSON SimpleVersion1 where
  version = 1
  kind = extension

instance Migrate SimpleVersion1 where
  type MigrateFrom SimpleVersion1 = UUID
  migrate uuid = SimpleVersion1 uuid ""

data BadVersion = BadVersion Text deriving (Eq, Show)
instance FromJSON BadVersion where parseJSON = withText "BadVersion" $ pure . BadVersion
instance ToJSON   BadVersion where toJSON (BadVersion t) = String t
instance SafeJSON BadVersion where version = 8

-- Used for the HashSet migration test
instance Hashable Version4 where
  hashWithSalt i = hashWithSalt i . encode
