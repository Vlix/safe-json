{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Data.SafeJSON.Instances
Copyright   : (c) 2019 Felix Paulusma
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : experimental

This module contains 'SafeJSON' instances for almost
all types that "Data.Aeson" has 'Data.Aeson.FromJSON'
and 'Data.Aeson.ToJSON' instances for. These instances
are all defined with 'noVersion' and 'base', since
these types should never get a version wrapper, should
use the existing "Data.Aeson" instances and do not extend
any other type.

All these types are extendable if need be. Just use
any of these types in the definition of your 'Migrate'
instance.

(e.g. @type MigrateFrom MyType = Int@)
-}
module Data.SafeJSON.Instances (SafeJSON(..)) where


import Control.Applicative (Const(..))
import Data.Aeson (DotNetTime, FromJSONKey, ToJSONKey, Value(..), parseJSON, toJSON)
import Data.Aeson.Types (Parser)
import Data.Char (Char)
import Data.DList as DList (DList, fromList)
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose) -- FIXME: add SafeJSON Instances
import Data.Functor.Product (Product) -- FIXME: add SafeJSON Instances
import Data.Functor.Sum (Sum(..))     -- FIXME: add SafeJSON Instances
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM (HashMap, fromList, toList)
import qualified Data.HashSet as HS (HashSet, fromList, toList)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Monoid (Dual(..))
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Scientific (Scientific)
import Data.Semigroup (First(..), Last(..), Max(..), Min(..))
import Data.Sequence (Seq)
import qualified Data.Set as S (Set, fromList, toList)
import Data.Text as T (Text)
import Data.Text.Lazy as LT (Text)
import Data.Time
import Data.Tree (Tree)
import Data.UUID.Types (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Version as DV (Version)
import Data.Void (Void)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CTime)
import Numeric.Natural (Natural)

import Data.SafeJSON.Internal


-- ---------------------- --
--   SafeJSON Instances   --
-- ---------------------- --

#define BASIC_NULLARY(T) \
instance SafeJSON T where { version = noVersion }

BASIC_NULLARY(Void)
BASIC_NULLARY(Bool)
BASIC_NULLARY(Ordering)
BASIC_NULLARY(())
BASIC_NULLARY(Char)
BASIC_NULLARY(Float)
BASIC_NULLARY(Double)
BASIC_NULLARY(Int)
BASIC_NULLARY(Natural)
BASIC_NULLARY(Integer)
BASIC_NULLARY(Int8)
BASIC_NULLARY(Int16)
BASIC_NULLARY(Int32)
BASIC_NULLARY(Int64)
BASIC_NULLARY(Word)
BASIC_NULLARY(Word8)
BASIC_NULLARY(Word16)
BASIC_NULLARY(Word32)
BASIC_NULLARY(Word64)
BASIC_NULLARY(T.Text)
BASIC_NULLARY(LT.Text)
BASIC_NULLARY(DV.Version)
BASIC_NULLARY(Scientific)
BASIC_NULLARY(IntSet)
BASIC_NULLARY(UUID)
BASIC_NULLARY(Value)

instance (SafeJSON a, Integral a) => SafeJSON (Ratio a) where
  typeName = typeName1
  version = noVersion

instance (HasResolution a) => SafeJSON (Fixed a) where
  typeName = typeName1
  version = noVersion

instance SafeJSON (Proxy a) where
  typeName = typeName1
  version = noVersion

instance {-# OVERLAPPING #-} SafeJSON String where
  typeName _ = "String"
  version = noVersion


-- --------------------------- --
--   SafeJSON Time Instances   --
-- --------------------------- --

BASIC_NULLARY(CTime)
BASIC_NULLARY(ZonedTime)
BASIC_NULLARY(LocalTime)
BASIC_NULLARY(TimeOfDay)
BASIC_NULLARY(UTCTime)
BASIC_NULLARY(NominalDiffTime)
BASIC_NULLARY(DiffTime)
BASIC_NULLARY(Day)
BASIC_NULLARY(DotNetTime)

-- ------------------------------------ --
--   More involved SafeJSON instances   --
-- ------------------------------------ --

instance SafeJSON a => SafeJSON (Const a b) where
  safeFrom val = contain $ Const <$> safeFromJSON val
  safeTo (Const a) = contain $ safeToJSON a
  typeName = typeName2
  version = noVersion

instance SafeJSON a => SafeJSON (Maybe a) where
  -- This follows the same 'Null' logic as the aeson library
  safeFrom Null = contain $ pure (Nothing :: Maybe a)
  safeFrom val = contain $ Just <$> safeFromJSON val
  -- Nothing means do whatever Aeson thinks Nothing should be
  safeTo Nothing = contain $ toJSON (Nothing :: Maybe a)
  -- If there's something, keep it safe
  safeTo (Just a) = contain $ safeToJSON a
  typeName = typeName1
  version = noVersion

instance (SafeJSON a, SafeJSON b) => SafeJSON (Either a b) where
  safeFrom val = contain $ do
      eVal <- parseJSON val
      case eVal of
        Left a  -> Left  <$> safeFromJSON a
        Right b -> Right <$> safeFromJSON b
  safeTo (Left a)  = contain $ toJSON (Left  $ safeToJSON a :: Either Value Void)
  safeTo (Right b) = contain $ toJSON (Right $ safeToJSON b :: Either Void Value)
  typeName = typeName2
  version = noVersion

#define BASIC_UNARY(T)                             \
instance SafeJSON a => SafeJSON (T a) where {      \
  safeFrom val = contain $ T <$> safeFromJSON val; \
  safeTo (T a) = contain $ safeToJSON a;           \
  typeName = typeName1;                            \
  version = noVersion }

BASIC_UNARY(Identity)
BASIC_UNARY(First)
BASIC_UNARY(Last)
BASIC_UNARY(Min)
BASIC_UNARY(Max)
BASIC_UNARY(Dual)

fromGenericVector :: (SafeJSON a, VG.Vector v a) => Value -> Contained (Parser (v a))
fromGenericVector val = contain $ do
      v <- parseJSON val
      VG.convert <$> VG.mapM safeFromJSON (v :: V.Vector Value)

toGenericVector :: (SafeJSON a, VG.Vector v a) => v a -> Contained Value
toGenericVector = contain . toJSON . fmap safeToJSON . VG.toList

instance SafeJSON a => SafeJSON (V.Vector a) where
  safeFrom = fromGenericVector
  safeTo = toGenericVector
  typeName = typeName1
  version = noVersion

instance (SafeJSON a, VP.Prim a) => SafeJSON (VP.Vector a) where
  safeFrom = fromGenericVector
  safeTo = toGenericVector
  typeName = typeName1
  version = noVersion

instance (SafeJSON a, VS.Storable a) => SafeJSON (VS.Vector a) where
  safeFrom = fromGenericVector
  safeTo = toGenericVector
  typeName = typeName1
  version = noVersion

instance (SafeJSON a, VG.Vector VU.Vector a) => SafeJSON (VU.Vector a) where
  safeFrom = fromGenericVector
  safeTo = toGenericVector
  typeName = typeName1
  version = noVersion

-- | Lists and any other "container" are seen as only that:
--   a container for 'SafeJSON' values.
--
--   "Containers" are implemented in such a way that when parsing
--   a collection of all migratable versions, the result will be
--   a list of that type where each element has been migrated as
--   appropriate.
instance  {-# OVERLAPPABLE #-} SafeJSON a => SafeJSON [a] where
  safeFrom val = contain $ do
      vs <- parseJSON val
      mapM safeFromJSON vs
  safeTo as = contain . toJSON $ safeToJSON <$> as
  typeName = typeName1
  version = noVersion

#define BASIC_UNARY_FUNCTOR(T)                      \
instance SafeJSON a => SafeJSON (T a) where {       \
  safeFrom val = contain $ do {                     \
      vs <- parseJSON val;                          \
      mapM safeFromJSON vs };                       \
  safeTo as = contain . toJSON $ safeToJSON <$> as; \
  typeName = typeName1;                             \
  version = noVersion }

BASIC_UNARY_FUNCTOR(IntMap)
BASIC_UNARY_FUNCTOR(NonEmpty)
BASIC_UNARY_FUNCTOR(Seq)
BASIC_UNARY_FUNCTOR(Tree)

instance (SafeJSON a) => SafeJSON (DList a) where
  safeFrom val = contain $ do
      vs <- parseJSON val
      DList.fromList <$> mapM safeFromJSON vs
  safeTo as = contain . toJSON $ safeToJSON <$> as
  typeName = typeName1
  version = noVersion

instance (SafeJSON a, Ord a) => SafeJSON (S.Set a) where
  safeFrom val = contain $ do
      vs <- parseJSON val
      S.fromList <$> safeFromJSON vs
  safeTo as = contain . toJSON $ safeToJSON <$> S.toList as
  typeName = typeName1
  version = noVersion

instance (Ord k, FromJSONKey k, ToJSONKey k, SafeJSON a) => SafeJSON (Map k a) where
  safeFrom val = contain $ do
      vs <- parseJSON val
      mapM safeFromJSON vs
  safeTo as = contain . toJSON $ safeToJSON <$> as
  typeName = typeName2
  version = noVersion

instance (SafeJSON a, Eq a, Hashable a) => SafeJSON (HS.HashSet a) where
  safeFrom val = contain $ do
      vs <- parseJSON val
      HS.fromList <$> safeFromJSON vs
  safeTo as = contain . toJSON $ safeToJSON <$> HS.toList as
  typeName = typeName1
  version = noVersion

instance (Hashable a, FromJSONKey a, ToJSONKey a, Eq a, SafeJSON b) => SafeJSON (HM.HashMap a b) where
  safeFrom val = contain $ do
      vs <- parseJSON val
      fmap HM.fromList . mapM (mapM safeFromJSON) $ HM.toList vs
  safeTo as = contain . toJSON $ safeToJSON <$> as
  typeName = typeName2
  version = noVersion

instance (SafeJSON a, SafeJSON b) => SafeJSON (a, b) where
  safeFrom x = contain $ do
      (a',b') <- parseJSON x
      a <- safeFromJSON a'
      b <- safeFromJSON b'
      return (a,b)
  safeTo (a,b) = contain $ toJSON (safeToJSON a, safeToJSON b)
  typeName = typeName2
  version = noVersion

instance (SafeJSON a, SafeJSON b, SafeJSON c) => SafeJSON (a, b, c) where
  safeFrom x = contain $ do
      (a',b',c') <- parseJSON x
      a <- safeFromJSON a'
      b <- safeFromJSON b'
      c <- safeFromJSON c'
      return (a,b,c)
  safeTo (a,b,c) = contain $ toJSON (safeToJSON a, safeToJSON b, safeToJSON c)
  typeName = typeName3
  version = noVersion

instance (SafeJSON a, SafeJSON b, SafeJSON c, SafeJSON d) => SafeJSON (a, b, c, d) where
  safeFrom x = contain $ do
      (a',b',c',d') <- parseJSON x
      a <- safeFromJSON a'
      b <- safeFromJSON b'
      c <- safeFromJSON c'
      d <- safeFromJSON d'
      return (a,b,c,d)
  safeTo (a,b,c,d) = contain $ toJSON (safeToJSON a, safeToJSON b, safeToJSON c, safeToJSON d)
  typeName = typeName4
  version = noVersion

instance (SafeJSON a, SafeJSON b, SafeJSON c, SafeJSON d, SafeJSON e) => SafeJSON (a, b, c, d, e) where
  safeFrom x = contain $ do
      (a',b',c',d',e') <- parseJSON x
      a <- safeFromJSON a'
      b <- safeFromJSON b'
      c <- safeFromJSON c'
      d <- safeFromJSON d'
      e <- safeFromJSON e'
      return (a,b,c,d,e)
  safeTo (a,b,c,d,e) = contain $ toJSON (safeToJSON a, safeToJSON b, safeToJSON c, safeToJSON d, safeToJSON e)
  typeName = typeName5
  version = noVersion
