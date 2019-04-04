{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Data.SafeJSON.Internal where


import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as HM (insert)
import Data.Int
import Data.List (nub)
import Data.Proxy
import Data.Text (Text)


newtype Contained a = Contained {unsafeUnpack :: a}

contain :: a -> Contained a
contain = Contained

-- TODO: Might be a good idea to take the 'Contained' approach that
-- SafeCopy also uses, so no one can use 'safeTo' and 'safeFrom' directly.
class (ToJSON a, FromJSON a) => SafeJSON a where

  version :: Version a
  version = 1

  kind :: Kind a
  kind = Base

  safeTo :: a -> Contained Value
  safeTo = contain . toJSON

  safeFrom :: Value -> Contained (Parser a)
  safeFrom = contain . parseJSON

  errorTypeName :: Proxy a -> String
  errorTypeName _ = "<unknown type>"

  internalConsistency :: Consistency a
  internalConsistency = computeConsistency Proxy

  objectProfile :: Profile a
  objectProfile = mkProfile Proxy


class SafeJSON (MigrateFrom a) => Migrate a where
  type MigrateFrom a
  migrate :: MigrateFrom a -> a

newtype Version a = Version {unVersion :: Int64}
  deriving (Eq, Show, Num)

castVersion :: Version a -> Version b
castVersion (Version i) = Version i

data Kind a where
  Primitive :: Kind a
  Base :: Kind a
  Extends :: Migrate a => Proxy (MigrateFrom a) -> Kind a


isPrimitive :: Kind a -> Bool
isPrimitive Primitive = True
isPrimitive _         = False

versionTag :: Text
versionTag = "_v"

dataVersionTag :: Text
dataVersionTag = "__v"

dataTag :: Text
dataTag = "_data"

primitive :: Kind a
primitive = Primitive

base :: Kind a
base = Base

extension :: (SafeJSON a, Migrate a) => Kind a
extension = Extends Proxy

-- | We don't check consistency here, since we're only adding a version number.
safeToJSON :: forall a. SafeJSON a => a -> Value
safeToJSON a = case kindFromProxy p of
    Primitive -> unsafeUnpack $ safeTo a
    _         -> case unsafeUnpack $ safeTo a of
        Object o -> Object $ HM.insert versionTag (toJSON i) o
        other    -> object [ dataVersionTag .= i
                           , dataTag        .= other
                           ]
  where Version i = version :: Version a
        p = Proxy :: Proxy a

-- TODO: Add fallback for version 0 in case no version is found.
-- This will be helpful in situations where this library is added later on.
-- (This is also the reason the default version in the 'SafeJSON' class is 1)
-- | Even though the consistency is checked, this shouldn't actually fail
-- because of consistency if you have a test-suite that checks the
-- consistency of all the types that have a SafeJSON instance.
safeFromJSON :: forall a. SafeJSON a => Value -> Parser a
safeFromJSON origVal = checkConsistency p $
    case kindFromProxy p of
      Primitive -> unsafeUnpack $ safeFrom origVal
      _         -> case origVal of
                      Object o -> firstTry o
                      _ -> safejsonErr $ "unparsable JSON value (not an object): "
                                      ++ errorTypeName p
  where p = Proxy :: Proxy a
        safejsonErr s = fail $ "safejson: " ++ s
        firstTry o = do
            mVersion <- o .:? versionTag
            case mVersion of
              Nothing -> tryOther o
              Just v -> withVersion (Version v) origVal
        withVersion v val = either parseErr id eResult
          where eResult = constructParserFromVersion val v $ kindFromVersion v
                parseErr e = safejsonErr $ mconcat
                    [ "couldn't parse with found version ("
                    , show v, "): ", e
                    ]
        tryOther o = do
            mVersion <- o .:? dataVersionTag
            case mVersion of
              Nothing -> safejsonErr "no version found"
              Just v -> do
                  mBody <- o .:? dataTag
                  let bodyErr = safejsonErr $
                        "no body found for JSON non-object (version " ++ show v ++ ")"
                  maybe bodyErr (withVersion $ Version v) mBody

constructParserFromVersion :: SafeJSON a => Value -> Version a -> Kind a -> Either String (Parser a)
constructParserFromVersion val origVersion origKind = worker origVersion origKind
  where
    worker :: SafeJSON b => Version b -> Kind b -> Either String (Parser b)
    worker thisVersion thisKind
      | version == thisVersion = return $ unsafeUnpack $ safeFrom val
      | otherwise = case thisKind of
          Primitive -> Left $ errorMsg thisKind "Cannot migrate from primitive types."
          Base      -> Left $ errorMsg thisKind versionNotFound
          Extends p -> fmap migrate <$> worker (castVersion origVersion) (kindFromProxy p)

    versionNotFound = "Cannot find parser associated with: " <> show origVersion
    errorMsg k msg = mconcat
        [ "safejson: "
        , errorTypeName (proxyFromKind k)
        , ": ", msg
        ]

proxyFromKind :: Kind a -> Proxy a
proxyFromKind _ = Proxy

kindFromProxy :: SafeJSON a => Proxy a -> Kind a
kindFromProxy _ = kind

kindFromVersion :: SafeJSON a => Version a -> Kind a
kindFromVersion _ = kind

versionFromProxy :: SafeJSON a => Proxy a -> Version a
versionFromProxy _ = version

versionFromKind :: SafeJSON a => Kind a -> Version a
versionFromKind _ = version


data Profile a = PrimitiveProfile
               | InvalidProfile String
               | Profile ProfileVersions

data ProfileVersions = ProfileVersions {
    profileCurrentVersion :: Int64,
    profileSupportedVersions :: [Int64]
  }

mkProfile :: SafeJSON a => Proxy a -> Profile a
mkProfile p = case computeConsistency p of
    NotConsistent t -> InvalidProfile t
    Consistent | isPrimitive (kindFromProxy p) -> PrimitiveProfile
    Consistent -> Profile $ ProfileVersions {
        profileCurrentVersion    = unVersion (versionFromProxy p),
        profileSupportedVersions = availableVersions p
      }


data Consistency a = Consistent
                   | NotConsistent String

checkConsistency :: (SafeJSON a, Monad m) => Proxy a -> m b -> m b
checkConsistency p m =
    case computeConsistency p of
      NotConsistent s -> fail s
      Consistent      -> m

computeConsistency :: SafeJSON a => Proxy a -> Consistency a
computeConsistency p
  | isObviouslyConsistent (kindFromProxy p) = Consistent
  | versions /= nub versions = NotConsistent $ "Duplicate version tags in chain: " ++ show versions
  | not (validChain p) = NotConsistent $ "Primitive types cannot be extended as they have no version tag."
  | otherwise = Consistent
  where versions = availableVersions p

isObviouslyConsistent :: Kind a -> Bool
isObviouslyConsistent Primitive = True
isObviouslyConsistent Base      = True
isObviouslyConsistent _         = False

availableVersions :: SafeJSON a => Proxy a -> [Int64]
availableVersions p = worker (kindFromProxy p)
  where
    worker :: SafeJSON b => Kind b -> [Int64]
    worker k = case k of
        Primitive  -> []
        Base       -> [v]
        Extends p' -> v : worker (kindFromProxy p')
      where Version v = versionFromKind k

validChain :: SafeJSON a => Proxy a -> Bool
validChain origProxy = case kindFromProxy origProxy of
    Extends p -> check (kindFromProxy p)
    _         -> True
  where
    check :: SafeJSON b => Kind b -> Bool
    check = \case
        Primitive -> False
        Base      -> True
        Extends p -> check (kindFromProxy p)
