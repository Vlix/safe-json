-- This module heavily relies on code borrowed from the "safecopy"
-- library by David Himmelstrup and Felipe Lessa, found on
-- "https://github.com/acid-state/safecopy"
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Data.SafeJSON.Internal where


import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Fail (MonadFail)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as HM (insert, size)
import Data.Int
import qualified Data.List as List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)


newtype Contained a = Contained {unsafeUnpack :: a}

contain :: a -> Contained a
contain = Contained

-- TODO: Explain how Version Nothing works.

-- Making SafeJSON instances for non-Object 'Value's
-- creates additional overhead (since they get turned into objects)
-- so it is advised to try to make SafeJSON instances only for
-- top-level types that contain other types.

-- While the minimal definition doesn't need any declarations,
-- it is advised to at least set the 'version' and 'kind'.
-- (and the typeName if your type is not Typeable)

-- SafeJSON will look forward once, but after that go down the chain.
class (ToJSON a, FromJSON a) => SafeJSON a where
  -- | The version of the type.
  --
  --   Only used as a key so it must be unique, (this is checked at run-time)
  --   but doesn't have to be sequential or continuous.
  --
  --   The default version is '0'.
  version :: Version a
  version = 0

  -- | The kind specifies how versions are dealt with. By default,
  --   values are tagged with version 0 and don't have any
  --   previous versions. See 'extension'.
  kind :: Kind a
  kind = Base

  -- | This method defines how a value should be serialized without worrying
  --   about adding the version. This function cannot be used directly.
  --   One should use 'safeToJSON', instead.
  safeTo :: a -> Contained Value
  safeTo = contain . toJSON

  -- | This method defines how a value should be parsed without also worrying
  --   about writing out the version tag. This function cannot be used directly.
  --   One should use 'safeFromJSON', instead.
  safeFrom :: Value -> Contained (Parser a)
  safeFrom = contain . parseJSON

  -- | The name of the type. This is only used in error
  --   message strings.
  --   Feel free to leave undefined in your instances.
  typeName :: Proxy a -> String
  default typeName :: Typeable a => Proxy a -> String
  typeName = typeName0

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

-- | This instance is needed to handle older versions.
--
--   /N.B. Where @Migrate a@ migrates from the previous
--   version to the type @a@, @Migrate (Reverse a)@ can
--   be read as @MigrateTo a@ because it is the inverse
--   of what @MigrateFrom@ does. (i.e. migrate from the
--   next version back to type @a@. This is useful when
--   needing in-place updating of message formats, for
--   example)/
class SafeJSON (MigrateFrom a) => Migrate a where
  type MigrateFrom a
  migrate :: MigrateFrom a -> a


-- | A simple numeric version id.
--
--   'Version' has a 'Num' instance and as such can be
--   declared using integer literals: @version = 2@
newtype Version a = Version {unVersion :: Maybe Int64}
-- Is it better to use 'Int32'?
-- Maybe 'Int64' is too big for JSON?
  deriving (Eq)

-- | 'noVersion' is used for types that don't have
--   a version tag. This is used for primitive values, like
--   'Int', 'Text', '[a]', etc.
--   But also when implementing 'SafeJSON' after the fact.
noVersion :: Version a
noVersion = Version Nothing

instance Show (Version a) where
  show (Version mi) = "Version " ++ showV mi

liftV, liftV' :: (Int64 -> Int64 -> Int64) -> Maybe Int64 -> Maybe Int64 -> Maybe Int64

liftV _ Nothing Nothing = Nothing
liftV f ma mb = Just $ toZ ma `f` toZ mb

liftV' _ Nothing Nothing = Nothing
liftV' f ma mb = Just $ toZ' ma `f` toZ' mb

-- | Nothing is handled as if it's zero.
instance Num (Version a) where
  Version ma + Version mb = Version $ liftV  (+) ma mb
  Version ma - Version mb = Version $ liftV  (-) ma mb
  Version ma * Version mb = Version $ liftV' (*) ma mb
  negate (Version ma) = Version $ negate <$> ma
  abs    (Version ma) = Version $ abs    <$> ma
  signum (Version ma) = Version $ signum <$> ma
  fromInteger i = Version $ Just $ fromInteger i

toZ, toZ' :: Num i => Maybe i -> i
toZ  = fromMaybe $ fromInteger 0
toZ' = fromMaybe $ fromInteger 1

castVersion :: Version a -> Version b
castVersion (Version i) = Version i

-- | This is a wrapper type used migrating backwards in the chain of compatible types.
--
--   This is useful when running updates in production where new-format JSON will be
--   received by old-format expecting services.
newtype Reverse a = Reverse { unReverse :: a }

-- | The kind of a data type determines how it is tagged (if at all).
--
--   Base kinds (see 'base') are at the bottom of the chain and can
--   optionally have no version tag. (@Base@ and @Extended Base@ are
--   the only kinds that can have no version)
--
--   Extensions (see 'extension') tells the system that there exists
--   a previous version of the data type which should be migrated if
--   needed. (This requires the data type to also have a 'Migrate a' instance)
--
--   Forward extensions (see 'extended_base' and 'extended_extension')
--   tell the system there exists at least a next version from which
--   the data type can be reverse-migrated.
--   (This requires the data type to also have a 'Migrate (Reverse a)' instance)
data Kind a where
  Base :: Kind a
  Extends :: Migrate a => Proxy (MigrateFrom a) -> Kind a
  Extended :: Migrate (Reverse a) => Kind a -> Kind a

base :: Kind a
base = Base

extension :: (SafeJSON a, Migrate a) => Kind a
extension = Extends Proxy

extended_extension :: (SafeJSON a, Migrate a, Migrate (Reverse a)) => Kind a
extended_extension = Extended extension

extended_base :: (SafeJSON a, Migrate (Reverse a)) => Kind a
extended_base = Extended base

-- The '!' and '~' used in these set fields are chosen for their
-- low probability of showing up naturally in JSON objects one
-- would normally find or construct.

versionField :: Text
versionField = "!v"

dataVersionField :: Text
dataVersionField = "~v"

dataField :: Text
dataField = "~d"

-- We don't check consistency here, since we're only adding a version number.
safeToJSON :: forall a. SafeJSON a => a -> Value
safeToJSON a = case thisKind of
    Base          | i == Nothing -> tojson
    Extended Base | i == Nothing -> tojson
    _ -> case tojson of
            Object o -> Object $ HM.insert versionField (toJSON i) o
            other    -> object
                [ dataVersionField .= i
                , dataField .= other
                ]
  where tojson = unsafeUnpack $ safeTo a
        Version i = version :: Version a
        thisKind = kind :: Kind a

-- | The consistency is not checked before every parsing.
--   This will either always run or always fail depending on
--   the consistency of the 'SafeJSON' instance in question,
--   since checkConsistency is
safeFromJSON :: forall a. SafeJSON a => Value -> Parser a
safeFromJSON origVal = checkConsistency p $
    case origKind of
      Base       | i == Nothing -> unsafeUnpack $ safeFrom origVal
      Extended k | i == Nothing -> extendedCase k
      _ -> regularCase
  where Version i = version :: Version a
        origKind = kind :: Kind a
        p = Proxy :: Proxy a
        safejsonErr s = fail $ "safejson: " ++ s

        regularCase = case origVal of
            Object o -> do
                (mVal, v) <- firstTry o <|> secondTry o <|> pure (Nothing, noVersion)
                let val = fromMaybe origVal mVal
                withVersion v val origKind
            _ -> withoutVersion <|> safejsonErr ("unparsable JSON value (not an object): " ++ typeName p)
          where withoutVersion = withVersion noVersion origVal origKind

        -- This only runs if the SafeJSON being tried has 'kind' of 'extended_*'
        -- and the version is 'noVersion'.
        -- (internalConsistency checks that it should be an 'Extended Base' if it has 'noVersion')
        -- We check the newer version first, since it's better to try to find the
        -- version, if there is one, to guarantee the right parser.
        extendedCase :: Migrate (Reverse a) => Kind a -> Parser a
        extendedCase k = case k of { Base -> go; _ -> regularCase }
          where go = case origVal of
                        Object o -> tryNew o <|> tryOrig
                        _ -> tryOrig
                tryNew o = do
                    (mVal, v) <- firstTry o <|> secondTry o
                    let forwardKind = getForwardKind k
                        forwardVersion = castVersion v
                        val = fromMaybe origVal mVal
                        getForwardParser = withVersion forwardVersion val forwardKind
                    unReverse . migrate <$> getForwardParser
                tryOrig = unsafeUnpack $ safeFrom origVal

        withVersion :: forall b. SafeJSON b => Version b -> Value ->  Kind b -> Parser b
        withVersion v val k = either parseErr id eResult
          where eResult = constructParserFromVersion val v k
                parseErr e = safejsonErr $ mconcat
                    ["couldn't parse with found version (", show v, "): ", e]

        firstTry o = do
            v <- o .: versionField
            return (Nothing, Version $ Just v)
        secondTry o = do
            v  <- o .: dataVersionField
            bd <- o .: dataField
            -- This is an extra counter measure against false parsing.
            -- The simple data object should contain exactly the
            -- (~v) and (~d) fields
            when (HM.size o /= 2) $ fail "not simple data"
            return (Just bd, Version $ Just v)

-- This takes the version number found (or Nothing) and tries find the type in
-- the chain that has that version number. It will attempt to go one type up
-- (try 'Migrate (Reverse a)' once) and after that down the chain.
constructParserFromVersion :: SafeJSON a => Value -> Version a -> Kind a -> Either String (Parser a)
constructParserFromVersion val origVersion origKind =
    worker False origVersion origKind
  where
    worker :: forall b. SafeJSON b => Bool -> Version b -> Kind b -> Either String (Parser b)
    worker fwd thisVersion thisKind
      | version == thisVersion = return $ unsafeUnpack $ safeFrom val
      | otherwise = case thisKind of
          Base          -> Left errorMsg
          Extended Base -> Left errorMsg
          Extends p     -> fmap migrate <$> worker fwd (castVersion thisVersion) (kindFromProxy p)
          Extended k    -> do
              let forwardParser :: Either String (Parser b)
                  forwardParser = fmap (unReverse . migrate) <$> worker True (castVersion thisVersion) (kindFromProxy reverseProxy)
                  previousParser :: Either String (Parser b)
                  previousParser = worker True thisVersion k
              if fwd || thisVersion == noVersion
                then previousParser
                else either (const previousParser) Right forwardParser
      where versionNotFound = "Cannot find parser associated with: " <> show origVersion
            errorMsg = mconcat ["safejson: ", typeName (Proxy :: Proxy b), ": ", versionNotFound]
            reverseProxy :: Proxy (MigrateFrom (Reverse b))
            reverseProxy = Proxy

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

getForwardKind :: Migrate (Reverse a) => Kind a -> Kind (MigrateFrom (Reverse a))
getForwardKind _ = kind

-- | Type name string representation of a nullary type constructor.
typeName0 :: Typeable a => Proxy a -> String
typeName0 = show . typeRep

-- | Type name string representation of a unary type constructor.
typeName1 :: forall t a. Typeable t => Proxy (t a) -> String
typeName1 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a binary type constructor.
typeName2 :: forall t a b. Typeable t => Proxy (t a b) -> String
typeName2 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a ternary type constructor.
typeName3 :: forall t a b c. Typeable t => Proxy (t a b c) -> String
typeName3 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a 4-ary type constructor.
typeName4 :: forall t a b c d. Typeable t => Proxy (t a b c d) -> String
typeName4 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a 5-ary type constructor.
typeName5 :: forall t a b c d e. Typeable t => Proxy (t a b c d e) -> String
typeName5 _ = show $ typeRep (Proxy :: Proxy t)


-- | Profile of the internal consistency of a 'SafeJSON' instance.
--
--   /N.B. 'noVersion' shows as 'null' instead of a number./
data Profile a = InvalidProfile String -- ^ There is something wrong with versioning
               | Profile ProfileVersions -- ^ Profile of consistent versions
  deriving (Eq)

-- | Version profile of a consistent 'SafeJSON' instance.
--
-- | 'Version Nothing' shows as 'null'
data ProfileVersions = ProfileVersions {
    profileCurrentVersion :: Maybe Int64,
    profileSupportedVersions :: [(Maybe Int64, String)]
  } deriving (Eq)

showV :: Maybe Int64 -> String
showV Nothing  = "null"
showV (Just i) = show i

showVs :: [(Maybe Int64, String)] -> String
showVs = List.intercalate ", " . fmap go
  where go (mi, s) = mconcat ["(", showV mi, ", ", s, ")"]

-- | 'Version Nothing' is shows as 'null'
instance Show ProfileVersions where
  show (ProfileVersions cur sup) = mconcat
      [ "version ", showV cur, ": ["
      , showVs sup, "]"
      ]

instance Typeable a => Show (Profile a) where
  show (InvalidProfile s) = "InvalidProfile: " <> s
  show (Profile pv) =
      let p = Proxy :: Proxy a
      in mconcat [ "Profile for \"", typeName0 p
                 , "\" (", show pv, ")"
                 ]

-- | Easy way to get a printable failure/success report
-- of the internal consistency of a SafeJSON instance.
mkProfile :: SafeJSON a => Proxy a -> Profile a
mkProfile p = case computeConsistency p of
    NotConsistent t -> InvalidProfile t
    Consistent -> Profile $ ProfileVersions {
        profileCurrentVersion    = unVersion (versionFromProxy p),
        profileSupportedVersions = availableVersions p
      }

data Consistency a = Consistent
                   | NotConsistent String

checkConsistency :: (SafeJSON a, MonadFail m) => Proxy a -> m b -> m b
checkConsistency p m =
    case consistentFromProxy p of
      NotConsistent s -> fail s
      Consistent      -> m

consistentFromProxy :: SafeJSON a => Proxy a -> Consistency a
consistentFromProxy _ = internalConsistency

computeConsistency :: SafeJSON a => Proxy a -> Consistency a
computeConsistency p
-- This checks the chain of versions to not clash or loop,
-- and it verifies only 'Base' or 'Extended Base' kinds can
-- have 'noVersion'
  | isObviouslyConsistent (kindFromProxy p) = Consistent
  | Just s <- invalidChain p = NotConsistent s
  | otherwise = Consistent
{-# INLINE computeConsistency #-}

isObviouslyConsistent :: Kind a -> Bool
isObviouslyConsistent Base = True
isObviouslyConsistent _    = False

availableVersions :: SafeJSON a => Proxy a -> [(Maybe Int64, String)]
availableVersions p =
    worker False (kindFromProxy p)
  where
    worker :: SafeJSON b => Bool -> Kind b -> [(Maybe Int64, String)]
    worker fwd thisKind = case thisKind of
        Base       -> [tup]
        Extends p' -> tup : worker fwd (kindFromProxy p')
        Extended k | not fwd -> worker True (getForwardKind k)
        Extended k -> worker True k

      where Version v = versionFromKind thisKind
            name = typeName $ proxyFromKind thisKind
            tup = (v, name)

-- TODO: Have this output a custom type to differentiate between bad outcomes.
-- That way the tests can be more reliable. (Did they catch what they were
-- supposed to catch?)
invalidChain :: SafeJSON a => Proxy a -> Maybe String
invalidChain a_proxy =
  worker mempty mempty (kindFromProxy a_proxy)
  where
    --                                Version set            Version set with type name     Kind      Maybe error
    worker :: forall a. SafeJSON a => S.Set (Maybe Int64) -> S.Set (Maybe Int64, String) -> Kind a -> Maybe String
    worker vs vSs k
      | i `S.member` vs = Just $ mconcat
          [ "Double occurence of version number '", showV i
          , "' (type: ", typeName p
          , "). Looping instances if the previous combination of type and version number are found here: "
          , showVs $ S.toList vSs
          ]
      | otherwise = case k of
          Base -> Nothing
          Extends{} | i == Nothing -> Just $ mconcat
              [ typeName p, " has defined 'version = noVersion', "
              , " but it's 'kind' definition is not 'base' or 'extended_base'"
              ]
          Extends b_proxy -> worker newVSet newVsSet (kindFromProxy b_proxy)
          Extended a_kind -> worker vs vSs a_kind
      where Version i = version :: Version a
            p = proxyFromKind k
            newVSet = S.insert i vs
            newVsSet = S.insert (i, typeName p) vSs
