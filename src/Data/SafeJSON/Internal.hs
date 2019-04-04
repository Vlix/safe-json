-- This module heavily relies on code borrowed from the "safecopy"
-- library by David Himmelstrup and Felipe Lessa, found on
-- "https://github.com/acid-state/safecopy"
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Data.SafeJSON.Internal where


import Control.Monad.Fail (MonadFail)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as HM (insert)
import Data.Int
import Data.List (nub)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)


newtype Contained a = Contained {unsafeUnpack :: a}

contain :: a -> Contained a
contain = Contained

-- TODO: Explain how version 0 works.
-- (version 0 can also be used to avoid clashes with
-- the "_v" or "__v" fields)
-- Making SafeJSON instances for non-Object 'Value's
-- creates additional overhead (since they get turned into objects)
-- so it is advised to try to make SafeJSON instances only for
-- top-level types that contain other types.
-- While the minimal definition doesn't need any declarations,
-- it is advised to at least set the 'version' and 'kind'.
-- (and the errorTypeName if your type is not Typeable)
class (ToJSON a, FromJSON a) => SafeJSON a where
  -- | The version of the type.
  --
  --   Only used as a key so it must be unique, (this is checked at run-time)
  --   but doesn't have to be sequential or continuous.
  --
  --   The default version is '1'. (N.B. version '0' is handled uniquely)
  version :: Version a
  version = 1

  -- | The kind specifies how versions are dealt with. By default,
  --   values are tagged with their version id and don't have any
  --   previous versions. See 'extension' and the much less used
  --   'primitive'.
  kind :: Kind a
  kind = Base

  -- | This method defines how a value should be serialized without worrying about
  --   previous versions or migrations. This function cannot be used directly.
  --   One should use 'safeGet', instead.
  safeTo :: a -> Contained Value
  safeTo = contain . toJSON

  -- | This method defines how a value should be parsed without also worrying
  --   about writing out the version tag. This function cannot be used directly.
  --   One should use 'safeFromJSON', instead.
  safeFrom :: Value -> Contained (Parser a)
  safeFrom = contain . parseJSON

  errorTypeName :: Proxy a -> String
  default errorTypeName :: Typeable a => Proxy a -> String
  errorTypeName = typeName

  -- | Internal function that should not be overrided.
  --   @Consistent@ if the version history is consistent
  --   (i.e. there are no duplicate version numbers) and
  --   the chain of migrations is valid.
  --
  --   This function is in the typeclass so that this
  --   information is calculated only once during the program
  --   lifetime, instead of everytime 'safeFrom' or 'safeTo' is used.
  internalConsistency :: Consistency a
  internalConsistency = computeConsistency Proxy

  -- | Version profile.
  --
  --   Useful when running tests.
  --   Shows the current version of the type and all supported
  --   versions it can migrate from.
  objectProfile :: Profile a
  objectProfile = mkProfile Proxy

  {-# MINIMAL #-}

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

-- We don't check consistency here, since we're only adding a version number.
safeToJSON :: forall a. SafeJSON a => a -> Value
safeToJSON a = case kindFromProxy p of
    Primitive     -> tojson
    Base | i == 0 -> tojson
    _ -> case tojson of
            Object o -> Object $ HM.insert versionTag (toJSON i) o
            other    -> object
                [ dataVersionTag .= i
                , dataTag        .= other
                ]
  where tojson = unsafeUnpack $ safeTo a
        Version i = version :: Version a
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
      Primitive     -> unsafeUnpack $ safeFrom origVal
      Base | i == 0 -> unsafeUnpack $ safeFrom origVal
      _ -> case origVal of
              Object o -> firstTry o
              _ -> safejsonErr $ "unparsable JSON value (not an object): " ++ errorTypeName p
  where Version i = version :: Version a
        p = Proxy :: Proxy a
        safejsonErr s = fail $ "safejson: " ++ s
        firstTry o = do
            mVersion <- o .:? versionTag
            case mVersion of
              Nothing -> tryOther o
              Just v -> withVersion (Version v) origVal
        withVersion v val = either parseErr id eResult
          where eResult = constructParserFromVersion val v $ kindFromVersion v
                parseErr e = safejsonErr $ mconcat
                    ["couldn't parse with found version (", show v, "): ", e]
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

typeName :: Typeable a => Proxy a -> String
typeName = show . typeRep

typeName1 :: forall t a. Typeable t => Proxy (t a) -> String
typeName1 _ = show $ typeRep (Proxy :: Proxy t)

typeName2 :: forall t a b. Typeable t => Proxy (t a b) -> String
typeName2 _ = show $ typeRep (Proxy :: Proxy t)

typeName3 :: forall t a b c. Typeable t => Proxy (t a b c) -> String
typeName3 _ = show $ typeRep (Proxy :: Proxy t)

typeName4 :: forall t a b c d. Typeable t => Proxy (t a b c d) -> String
typeName4 _ = show $ typeRep (Proxy :: Proxy t)

typeName5 :: forall t a b c d e. Typeable t => Proxy (t a b c d e) -> String
typeName5 _ = show $ typeRep (Proxy :: Proxy t)


data Profile a = PrimitiveProfile
               | InvalidProfile String
               | Profile ProfileVersions

data ProfileVersions = ProfileVersions {
    profileCurrentVersion :: Int64,
    profileSupportedVersions :: [(Int64, String)]
  }

instance Typeable a => Show (Profile a) where
  show PrimitiveProfile   = "PrimitiveProfile"
  show (InvalidProfile s) = "InvalidProfile: " <> s
  show (Profile (ProfileVersions cur sup)) =
      let p = Proxy :: Proxy a
      in mconcat [ "Profile for \"", typeName p
                 , "\" (version ", show cur, "): "
                 , show sup
                 ]


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

checkConsistency :: (SafeJSON a, MonadFail m) => Proxy a -> m b -> m b
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

availableVersions :: SafeJSON a => Proxy a -> [(Int64, String)]
availableVersions p = worker (kindFromProxy p)
  where
    worker :: SafeJSON b => Kind b -> [(Int64, String)]
    worker k = case k of
        Primitive  -> []
        Base       -> [tup]
        Extends p' -> tup : worker (kindFromProxy p')
      where Version v = versionFromKind k
            name = errorTypeName $ proxyFromKind k
            tup = (v, name)

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
