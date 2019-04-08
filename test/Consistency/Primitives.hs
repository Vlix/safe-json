{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Consistency.Primitives where


import Control.Applicative (Const)
import Data.Aeson (DotNetTime, Value)
import Data.Aeson.Types (parseEither)
import Data.Char (Char)
import Data.DList (DList)
import Data.Fixed (E12, Fixed)
import Data.Functor.Identity (Identity)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Monoid (Dual)
import Data.Proxy (Proxy)
import Data.Ratio (Ratio)
import Data.Scientific (Scientific)
import Data.Semigroup (First, Last, Max, Min)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text as T (Text)
import Data.Text.Lazy as LT (Text)
import Data.Time
import Data.Tree (Tree)
import Data.UUID.Types (UUID)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Version as DV (Version)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CTime)
import Numeric.Natural (Natural)

import Test.Tasty
import Test.Tasty.QuickCheck (Arbitrary(..), testProperty)

import Data.SafeJSON
import Instances()


primitiveConsistency :: TestTree
primitiveConsistency = testGroup "Primitives conversions"
  [ testToFrom @Bool           "Bool"
  , testToFrom @Ordering       "Ordering"
  , testToFrom @()             "()"
  , testToFrom @Char           "Char"
  , testToFrom @Float          "Float"
  , testToFrom @Double         "Double"
  , testToFrom @Int            "Int"
  , testToFrom @Natural        "Natural"
  , testToFrom @Integer        "Integer"
  , testToFrom @Int8           "Int8"
  , testToFrom @Int16          "Int16"
  , testToFrom @Int32          "Int32"
  , testToFrom @Int64          "Int64"
  , testToFrom @Word           "Word"
  , testToFrom @Word8          "Word8"
  , testToFrom @Word16         "Word16"
  , testToFrom @Word32         "Word32"
  , testToFrom @Word64         "Word64"
  , testToFrom @T.Text         "T.Text"
  , testToFrom @LT.Text        "LT.Text"
  , testToFrom @DV.Version     "DV.Version"
  , testToFrom @Scientific     "Scientific"
  , testToFrom @IntSet         "IntSet"
  , testToFrom @UUID           "UUID"
  , testToFrom @CTime          "CTime"
  -- , testToFrom @ZonedTime      "ZonedTime" -- Apparently, there's a reason there's no Eq instance
  , testToFrom @LocalTime      "LocalTime"
  , testToFrom @TimeOfDay      "TimeOfDay"
  , testToFrom @UTCTime        "UTCTime"
  , testToFrom @NominalDiffTime"NominalDiffTime"
  , testToFrom @DiffTime       "DiffTime"
  , testToFrom @Day            "Day"
  , testToFrom @DotNetTime     "DotNetTime"
  , testToFrom @Value          "Value"

  , testToFrom @(Ratio Int)       "Ratio"
  , testToFrom @(Fixed E12)       "Fixed"
  , testToFrom @(Proxy ())        "Proxy"
  , testToFrom @(Identity T.Text) "Identity"
  , testToFrom @(First T.Text)    "First"
  , testToFrom @(Last T.Text)     "Last"
  , testToFrom @(Min T.Text)      "Min"
  , testToFrom @(Max T.Text)      "Max"
  , testToFrom @(Dual T.Text)     "Dual"
  , testToFrom @([Int])           "[]"
  , testToFrom @(IntMap Bool)     "IntMap"
  , testToFrom @(NonEmpty Int)    "NonEmpty"
  , testToFrom @(Seq T.Text)      "Seq"
  , testToFrom @(Tree T.Text)     "Tree"
  , testToFrom @(Const T.Text ())    "Const"
  , testToFrom @(Maybe T.Text)       "Maybe"
  , testToFrom @(Maybe T.Text)       "Maybe2"
  , testToFrom @(Either T.Text Bool) "Either"
  , testToFrom @(Either T.Text Bool) "Either2"
  , testToFrom @(DList Int)          "DList"
  , testToFrom @(V.Vector Int)       "V.Vector"
  , testToFrom @(VS.Vector Int)      "VS.Vector"
  , testToFrom @(VP.Vector Int)      "VP.Vector"
  , testToFrom @(VU.Vector Int)      "VU.Vector"
  , testToFrom @(Set Int)            "Set"
  , testToFrom @(Map T.Text Int)     "Map"
  , testToFrom @(HashSet Int)        "HashSet"
  , testToFrom @(HashMap T.Text Int) "HashMap"
  , testToFrom @(Int, Bool)                        "Tuple2"
  , testToFrom @(Int, Bool, T.Text)                "Tuple3"
  , testToFrom @(Int, Bool, T.Text, [Int])         "Tuple4"
  , testToFrom @(Int, Bool, T.Text, [Int], Double) "Tuple5"
  ]

testToFrom :: forall a. (SafeJSON a, Arbitrary a, Eq a, Show a) => String -> TestTree
testToFrom s = testProperty s $ \a ->
    Right (a :: a) == parseEither (safeFromJSON . safeToJSON) a
