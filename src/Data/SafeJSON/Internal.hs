-- This module heavily relies on code borrowed from the "safecopy"
-- library by David Himmelstrup and Felipe Lessa, found on
-- "https://github.com/acid-state/safecopy"
--
-- Though it has gone through extensive refactoring because of
-- desired behaviour being different from the safecopy library
-- and the fact that this library works with JSON, instead of
-- byte serialization.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-|
Module      : Data.SafeJSON.Internal
Copyright   : (c) 2019 Felix Paulusma
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : experimental

This module contains all "under-the-hood" functions
and types. "Data.SafeJSON" exports everything for the
outward-facing API.
-}
module Data.SafeJSON.Internal where


import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Fail (MonadFail)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict as HM (insert, size)
import Data.Int
import qualified Data.List as List (intercalate, lookup)
import Data.Maybe (fromMaybe, isJust, isNothing)
#if MIN_VERSION_base(4,11,0)
#else
import Data.Monoid ((<>))
#endif
import Data.Proxy
import qualified Data.Set as S
import Data.Text (Text)
import Data.Typeable (Typeable, typeRep)
import Test.Tasty.QuickCheck (Arbitrary(..), shrinkIntegral)


-- | A type that can be converted from and to JSON with versioning baked
--   in, using 'Migrate' to automate migration between versions, reducing
--   headaches when the need arrises to modify JSON formats while old
--   formats can't simply be disregarded.
class (ToJSON a, FromJSON a) => SafeJSON a where
  -- | The version of the type.
  --
  --   Only used as a key so it __must be unique__ (this is checked at run-time)
  --
  --   Version numbering __doesn't have to be sequential or continuous__.
  --
  --   /The default version is 0 (zero)./
  version :: Version a
  version = 0

  -- | The kind specifies how versions are dealt with. By default,
  --   values are tagged with version 0 and don't have any
  --   previous versions.
  --
  --   /The default kind is/ 'base'
  kind :: Kind a
  kind = Base

  -- | This method defines how a value should be serialized without worrying
  --   about adding the version. The default implementation uses 'toJSON', but
  --   can be modified if need be.
  --
  --   This function cannot be used directly. Use 'safeToJSON', instead.
  safeTo :: a -> Contained Value
  safeTo = contain . toJSON

  -- | This method defines how a value should be parsed without also worrying
  --   about writing out the version tag. The default implementation uses 'parseJSON',
  --   but can be modified if need be.
  --
  --   This function cannot be used directly. Use 'safeFromJSON', instead.
  safeFrom :: Value -> Contained (Parser a)
  safeFrom = contain . parseJSON

  -- | The name of the type. This is used in error message strings and the
  --   'Profile' report.
  --
  --   Doesn't have to be defined if your type is 'Data.Typeable.Typeable'. The default
  --   implementation is 'typeName0'. (cf. 'typeName1', 'typeName2', etc.)
  typeName :: Proxy a -> String
  default typeName :: Typeable a => Proxy a -> String
  typeName = typeName0

  --   Internal function that should not be overrided.
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
  --   Shows the current version of the type and all supported
  --   versions it can migrate from.
  objectProfile :: Profile a
  objectProfile = mkProfile Proxy

  {-# MINIMAL #-}

-- | This instance is needed to handle the migration between
--   older and newer versions.
--
--   Note that, where @(Migrate a)@ migrates from the previous
--   version to the type @a@, @(Migrate (Reverse a))@ migrates
--   from the future version to the type @a@.
--
-- === __Example__
--
-- __Two types that can migrate to each other.__
--
-- (Don't forget to give @OldType@ one of the @extended@ 'kind's,
-- and @NewType@ one of the @extension@ kinds.)
--
-- @
-- instance 'Migrate' NewType where
--   type 'MigrateFrom' NewType = OldType
--   'migrate' OldType = NewType
--
-- instance 'Migrate' (Reverse OldType) where
--   type 'MigrateFrom' (Reverse OldType) = NewType
--   'migrate' NewType = Reverse OldType
-- @
class SafeJSON (MigrateFrom a) => Migrate a where
  -- | The type from which will be migrated to type @a@
  type MigrateFrom a
  -- | The migration from the previous version to the
  --   current type @a@. OR, in case of a @(Reverse a)@,
  --   the migration from the future version back to
  --   the current type @a@
  migrate :: MigrateFrom a -> a


-- | This is an inpenetrable container. A security measure
--   used to ensure 'safeFrom' and 'safeTo' are never used
--   directly. Instead, always use 'safeFromJSON' and
--   'safeToJSON'.
newtype Contained a = Contained {unsafeUnpack :: a}

-- | Used when defining 'safeFrom' or 'safeTo'.
contain :: a -> Contained a
contain = Contained


-- | A simple numeric version id.
--
--   'Version' has a 'Num' instance and should be
--   declared using integer literals: @version = 2@
newtype Version a = Version {unVersion :: Maybe Int64}
-- Is it better to use 'Int32'?
-- Maybe 'Int64' is too big for JSON?
  deriving (Eq)

-- | This is used for types that don't have
--   a version tag.
--
--   This is used for primitive values that are tagged with
--   a version number, like 'Int', 'Text', @[a]@, etc.
--
--   But also when implementing 'SafeJSON' after the fact,
--   when a format is already in use, but you still want to
--   be able to 'migrate' from it to a newer type or format.
--
--   /N.B./ @version = noVersion@ /is distinctively different/
--   /from/ @version = 0@/, which will add a version tag with/
--   /the number 0 (zero), whereas/ 'noVersion' /will not add a/
--   /version tag./
noVersion :: Version a
noVersion = Version Nothing

instance Show (Version a) where
  show (Version mi) = "Version " ++ showV mi

liftV :: Integer -> (Int64 -> Int64 -> Int64) -> Maybe Int64 -> Maybe Int64 -> Maybe Int64
liftV _ _ Nothing Nothing = Nothing
liftV i f ma mb = Just $ toZ ma `f` toZ mb
  where toZ = fromMaybe $ fromInteger i

-- 'Version Nothing' is handled as if it's mempty... mostly.
-- | It is strongly discouraged to use any methods other
--   than 'fromInteger' of 'Version' 's 'Num' instance.
instance Num (Version a) where
  Version ma + Version mb = Version $ liftV 0 (+) ma mb
  Version ma - Version mb = Version $ liftV 0 (-) ma mb
  Version ma * Version mb = Version $ liftV 1 (*) ma mb
  negate (Version ma) = Version $ negate <$> ma
  abs    (Version ma) = Version $ abs    <$> ma
  signum (Version ma) = Version $ signum <$> ma
  fromInteger i = Version $ Just $ fromInteger i

-- | This instance explicitly doesn't consider 'noVersion', since it
-- is an exception in almost every sense.
instance Arbitrary (Version a) where
  arbitrary = Version . Just <$> arbitrary
  shrink (Version Nothing) = []
  shrink (Version (Just a)) = Version . Just <$> shrinkIntegral a

castVersion :: Version a -> Version b
castVersion (Version i) = Version i

-- | This is a wrapper type used migrating backwards in the chain of compatible types.
--
--   This is useful when running updates in production where new-format JSON will be
--   received by old-format expecting programs.
newtype Reverse a = Reverse { unReverse :: a }

-- | The 'kind' of a 'SafeJSON' type determines how it can be migrated to.
data Kind a where
  Base :: Kind a
  Extends :: Migrate a => Proxy (MigrateFrom a) -> Kind a
  Extended :: Migrate (Reverse a) => Kind a -> Kind a

-- | Used to define 'kind'.
--   @Base@ types do not extend any type.
base :: Kind a
base = Base

-- | Used to define 'kind'.
--   Extends a previous version.
extension :: (SafeJSON a, Migrate a) => Kind a
extension = Extends Proxy

-- | Used to define 'kind'.
--   Types that are 'extended_base', are extended by a
--   future version and as such can migrate backward from
--   that future version. (cf. 'extended_extension', 'base')
extended_base :: (SafeJSON a, Migrate (Reverse a)) => Kind a
extended_base = Extended base

-- | Used to define 'kind'.
--   Types that are 'extended_extension' are extended
--   by a future version and as such can migrate from
--   that future version, but they also extend a previous
--   version. (cf. 'extended_base', 'extension')
extended_extension :: (SafeJSON a, Migrate a, Migrate (Reverse a)) => Kind a
extended_extension = Extended extension

-- The '!' and '~' used in these set fields are chosen for their
-- low probability of showing up naturally in JSON objects one
-- would normally find or construct.

versionField :: Text
versionField = "!v"

dataVersionField :: Text
dataVersionField = "~v"

dataField :: Text
dataField = "~d"

-- | Use this exactly how you would use 'toJSON' from "Data.Aeson".
--   Though most use cases will probably use one of the 'Data.Aeson.Safe.encode'
--   functions from "Data.Aeson.Safe".
--
--   'safeToJSON' will add a version tag to the 'Value' created.
--   If the 'Value' resulting from 'safeTo' (by default the same as 'toJSON')
--   is an @Object@, an extra field with the version number will be added.
--
-- > Example value:
-- >   {"type":"test", "data":true}
-- >
-- > Resulting object:
-- >   {"!v": 1, "type":"test", "data":true}
--
--   If the resulting 'Value' is not an @Object@, it will be wrapped
--   in one, with a version field:
--
-- > Example value:
-- >   "arbitrary string"
-- >
-- > Resulting object:
-- >   {"~v": 1, "~d": "arbitrary string"}
--
--   __This function does not check consistency of the 'SafeJSON' instances.__
--   __It is advised to always 'Data.SafeJSON.Test.testConsistency' for all__
--   __your instances in a production setting.__
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

-- The consistency is checked on first parse, after that
-- there is no overhead.
-- | Use this exactly how you would use 'parseJSON' from "Data.Aeson".
--   Though most use cases will probably use one of the 'Data.Aeson.Safe.decode'
--   functions from "Data.Aeson.Safe".
--
--   'safeFromJSON' tries to find the version number in the JSON
--   'Value' provided, find the appropriate parser and migrate the
--   parsed result back to the requested type using 'Migrate'
--   instances.
--
--   If there is no version number (that means this can also happen with
--   completely unrelated JSON messages), and there is a 'SafeJSON'
--   instance in the chain that has 'version' defined as 'noVersion',
--   it will try to parse that type.
--
--   __N.B. If the consistency of the 'SafeJSON' instance in__
--   __question is faulty, this will always fail.__
safeFromJSON :: forall a. SafeJSON a => Value -> Parser a
safeFromJSON origVal = checkConsistency p $ \vs -> do
    let hasVNil = noVersionPresent vs
    case origKind of
      Base       | i == Nothing -> unsafeUnpack $ safeFrom origVal
      Extended k | i == Nothing -> extendedCase hasVNil k
      _ -> regularCase hasVNil
  where Version i = version :: Version a
        origKind = kind :: Kind a
        p = Proxy :: Proxy a
        safejsonErr s = fail $ "safejson: " ++ s
        regularCase hasVNil = case origVal of
            Object o -> do
                (mVal, v) <- tryIt o
                let val = fromMaybe origVal mVal
                withVersion v val origKind
            _ -> withoutVersion <|> safejsonErr ("unparsable JSON value (not an object): " ++ typeName p)
          where withoutVersion = withVersion noVersion origVal origKind
                tryIt o
                  | hasVNil = firstTry o <|> secondTry o <|> pure (Nothing, noVersion)
                  | otherwise = firstTry o <|> secondTry o

        -- This only runs if the SafeJSON being tried has 'kind' of 'extended_*'
        -- and the version is 'noVersion'.
        -- (internalConsistency checks that it should be an 'Extended Base' since it has 'noVersion')
        -- We check the newer version first, since it's better to try to find the
        -- version, if there is one, to guarantee the right parser.
        extendedCase :: Migrate (Reverse a) => Bool -> Kind a -> Parser a
        extendedCase hasVNil k = case k of { Base -> go; _ -> regularCase hasVNil }
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
        withVersion v val k = either fail id eResult
          where eResult = constructParserFromVersion val v k

        firstTry o = do
            v <- o .: versionField
            return (Nothing, Version $ Just v)
        secondTry o = do
            v  <- o .: dataVersionField
            bd <- o .: dataField
            -- This is an extra counter measure against false parsing.
            -- The simple data object should contain exactly the
            -- (~v) and (~d) fields
            when (HM.size o /= 2) $ fail $ "malformed simple data (" ++ show (Version $ Just v) ++ ")"
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
          Base          -> Left versionNotFound
          Extends p     -> fmap migrate <$> worker fwd (castVersion thisVersion) (kindFromProxy p)
          Extended k    -> do
              -- Technically, the forward and backward parsing could be
              -- infinite, as long as all 'Migrate' instances are defined.
              -- The problem is that chains can fork if, after going forward,
              -- the kind of that forward type is used to continue, since
              -- there's no guarantee that the migrations will continue backward
              -- down the previous chain.
              --
              -- TODO: Somehow restrict Migrate instances in such a way that, if defined:
              -- > MigrateFrom (Reverse b) = a
              -- >  THEN ALSO
              -- > MigrateFrom a = b
              --
              -- @
              -- v1 Base   v1' Base      v1'' Ext_Base
              --  |         |            /\
              --  |         |             |
              -- \/        \/            \/
              -- v2 Exs -> v3 Ext_Exs -> v4 Exs
              -- @
              --
              -- I've opted for the following approach:
              -- "Try forward once, if the version is wrong, go down your own chain"
              --
              -- IDEA: Maybe it could be written in such a way that the backward type
              -- (Base or Extends) in the Extended data constructor is passed along on
              -- up the chain until the top is reached, after which the run downward
              -- starts with Extends, or the run ends in case it was a Base type.
              let forwardParser :: Either String (Parser b)
                  forwardParser = do
                      if castVersion thisVersion == versionFromProxy reverseProxy
                          then Right $ unReverse . migrate <$> unsafeUnpack (safeFrom val)
                          else previousParser

                  previousParser :: Either String (Parser b)
                  previousParser = worker True thisVersion k
              -- If we've already looked ahead, or if it's 'noVersion', we go back.
              -- ('noVersion' means we need to find the 'Base', that's always backwards)
              if fwd || thisVersion == noVersion
                then previousParser
                else either (const previousParser) Right forwardParser
      where versionNotFound = "Cannot find parser associated with: " <> show origVersion
            reverseProxy :: Proxy (MigrateFrom (Reverse b))
            reverseProxy = Proxy

-- | Type name string representation of a __nullary__ type constructor.
typeName0 :: Typeable a => Proxy a -> String
typeName0 = show . typeRep

-- | Type name string representation of a __unary__ type constructor.
typeName1 :: forall t a. Typeable t => Proxy (t a) -> String
typeName1 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a __binary__ type constructor.
typeName2 :: forall t a b. Typeable t => Proxy (t a b) -> String
typeName2 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a __ternary__ type constructor.
typeName3 :: forall t a b c. Typeable t => Proxy (t a b c) -> String
typeName3 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a __4-ary__ type constructor.
typeName4 :: forall t a b c d. Typeable t => Proxy (t a b c d) -> String
typeName4 _ = show $ typeRep (Proxy :: Proxy t)

-- | Type name string representation of a __5-ary__ type constructor.
typeName5 :: forall t a b c d e. Typeable t => Proxy (t a b c d e) -> String
typeName5 _ = show $ typeRep (Proxy :: Proxy t)


-- | Profile of the internal consistency of a 'SafeJSON' instance.
--
--   /N.B. 'noVersion' shows as/ @null@ /instead of a number./
data Profile a = InvalidProfile String -- ^ There is something wrong with versioning
               | Profile ProfileVersions -- ^ Profile of consistent versions
  deriving (Eq)

-- | Version profile of a consistent 'SafeJSON' instance.
data ProfileVersions = ProfileVersions {
    profileCurrentVersion :: Maybe Int64, -- ^ Version of the type checked for consistency.
    profileSupportedVersions :: [(Maybe Int64, String)] -- ^ All versions in the chain with their type names.
  } deriving (Eq)

noVersionPresent :: ProfileVersions -> Bool
noVersionPresent (ProfileVersions c vs) =
    isNothing c || isJust (Nothing `List.lookup` vs)

showV :: Maybe Int64 -> String
showV Nothing  = "null"
showV (Just i) = show i

showVs :: [(Maybe Int64, String)] -> String
showVs = List.intercalate ", " . fmap go
  where go (mi, s) = mconcat ["(", showV mi, ", ", s, ")"]

-- | @Version Nothing@ shows as @null@
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
mkProfile :: forall a. SafeJSON a => Proxy a -> Profile a
mkProfile p = case computeConsistency p of
    NotConsistent t -> InvalidProfile t
    Consistent -> Profile $ ProfileVersions {
        profileCurrentVersion    = unVersion (version @a),
        profileSupportedVersions = availableVersions p
      }

data Consistency a = Consistent
                   | NotConsistent String

checkConsistency :: (SafeJSON a, MonadFail m) => Proxy a -> (ProfileVersions -> m b) -> m b
checkConsistency p m =
    case mkProfile p of
      InvalidProfile s -> fail s
      Profile vs -> m vs

computeConsistency :: forall a. SafeJSON a => Proxy a -> Consistency a
computeConsistency p
-- This checks the chain of versions to not clash or loop,
-- and it verifies only 'Base' or 'Extended Base' kinds can
-- have 'noVersion'
  | isObviouslyConsistent (kind @a) = Consistent
  | Just s <- invalidChain p = NotConsistent s
  | otherwise = Consistent
{-# INLINE computeConsistency #-}

isObviouslyConsistent :: Kind a -> Bool
isObviouslyConsistent Base = True
isObviouslyConsistent _    = False

availableVersions :: forall a. SafeJSON a => Proxy a -> [(Maybe Int64, String)]
availableVersions _ =
    worker False (kind @a)
  where
    worker :: forall b. SafeJSON b => Bool -> Kind b -> [(Maybe Int64, String)]
    worker fwd thisKind = case thisKind of
        Base       -> [tup]
        Extends p' -> tup : worker fwd (kindFromProxy p')
        Extended k | not fwd -> worker True (getForwardKind k)
        Extended k -> worker True k

      where Version v = version @b
            name = typeName (Proxy @b)
            tup = (v, name)

-- TODO: Have this output a custom type to differentiate between bad outcomes.
-- That way the tests can be more reliable. (Did they catch what they were
-- supposed to catch?)
invalidChain :: forall a. SafeJSON a => Proxy a -> Maybe String
invalidChain _ =
  worker mempty mempty (kind @a)
  where
    --                                Version set            Version set with type name     Kind      Maybe error
    worker :: forall b. SafeJSON b => S.Set (Maybe Int64) -> S.Set (Maybe Int64, String) -> Kind b -> Maybe String
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
          Extends a_proxy -> worker newVSet newVsSet (kindFromProxy a_proxy)
          Extended a_kind -> let v@(Version i') = versionFromKind $ getForwardKind a_kind
                                 tup = (i', typeName (proxyFromVersion v))
                              in worker (S.insert i' vs) (S.insert tup vSs) a_kind
      where Version i = version @b
            p = Proxy @b
            newVSet = S.insert i vs
            newVsSet = S.insert (i, typeName p) vSs


----------------------------------------------------------
-- Conversion functions
----------------------------------------------------------

proxyFromVersion :: Version a -> Proxy a
proxyFromVersion _ = Proxy

kindFromProxy :: SafeJSON a => Proxy a -> Kind a
kindFromProxy _ = kind

versionFromProxy :: SafeJSON a => Proxy a -> Version a
versionFromProxy _ = version

versionFromKind :: SafeJSON a => Kind a -> Version a
versionFromKind _ = version

getForwardKind :: Migrate (Reverse a) => Kind a -> Kind (MigrateFrom (Reverse a))
getForwardKind _ = kind
