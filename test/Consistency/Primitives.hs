{-# LANGUAGE TypeApplications #-}
module Consistency.Primitives where


import Control.Applicative (Const)
import Data.Aeson (DotNetTime, Value)
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

import Data.SafeJSON()
import Data.SafeJSON.Test (testRoundTripProp')
import Instances()


primitiveConsistency :: TestTree
primitiveConsistency = testGroup "Primitives conversions"
  [ testRoundTripProp' @Bool           "Bool"
  , testRoundTripProp' @Ordering       "Ordering"
  , testRoundTripProp' @()             "()"
  , testRoundTripProp' @Char           "Char"
  , testRoundTripProp' @Float          "Float"
  , testRoundTripProp' @Double         "Double"
  , testRoundTripProp' @Int            "Int"
  , testRoundTripProp' @Natural        "Natural"
  , testRoundTripProp' @Integer        "Integer"
  , testRoundTripProp' @Int8           "Int8"
  , testRoundTripProp' @Int16          "Int16"
  , testRoundTripProp' @Int32          "Int32"
  , testRoundTripProp' @Int64          "Int64"
  , testRoundTripProp' @Word           "Word"
  , testRoundTripProp' @Word8          "Word8"
  , testRoundTripProp' @Word16         "Word16"
  , testRoundTripProp' @Word32         "Word32"
  , testRoundTripProp' @Word64         "Word64"
  , testRoundTripProp' @T.Text         "T.Text"
  , testRoundTripProp' @LT.Text        "LT.Text"
  , testRoundTripProp' @DV.Version     "DV.Version"
  , testRoundTripProp' @Scientific     "Scientific"
  , testRoundTripProp' @IntSet         "IntSet"
  , testRoundTripProp' @UUID           "UUID"
  , testRoundTripProp' @CTime          "CTime"
  -- , testRoundTripProp' @ZonedTime      "ZonedTime" -- Apparently, there's a reason there's no Eq instance
  , testRoundTripProp' @LocalTime      "LocalTime"
  , testRoundTripProp' @TimeOfDay      "TimeOfDay"
  , testRoundTripProp' @UTCTime        "UTCTime"
  , testRoundTripProp' @NominalDiffTime"NominalDiffTime"
  , testRoundTripProp' @DiffTime       "DiffTime"
  , testRoundTripProp' @Day            "Day"
  , testRoundTripProp' @DotNetTime     "DotNetTime"
  , testRoundTripProp' @Value          "Value"

  , testRoundTripProp' @(Ratio Int)       "Ratio"
  , testRoundTripProp' @(Fixed E12)       "Fixed"
  , testRoundTripProp' @(Proxy ())        "Proxy"
  , testRoundTripProp' @(Identity T.Text) "Identity"
  , testRoundTripProp' @(First T.Text)    "First"
  , testRoundTripProp' @(Last T.Text)     "Last"
  , testRoundTripProp' @(Min T.Text)      "Min"
  , testRoundTripProp' @(Max T.Text)      "Max"
  , testRoundTripProp' @(Dual T.Text)     "Dual"
  , testRoundTripProp' @([Int])           "[]"
  , testRoundTripProp' @(IntMap Bool)     "IntMap"
  , testRoundTripProp' @(NonEmpty Int)    "NonEmpty"
  , testRoundTripProp' @(Seq T.Text)      "Seq"
  , testRoundTripProp' @(Tree T.Text)     "Tree"
  , testRoundTripProp' @(Const T.Text ())    "Const"
  , testRoundTripProp' @(Maybe T.Text)       "Maybe"
  , testRoundTripProp' @(Maybe T.Text)       "Maybe2"
  , testRoundTripProp' @(Either T.Text Bool) "Either"
  , testRoundTripProp' @(Either T.Text Bool) "Either2"
  , testRoundTripProp' @(DList Int)          "DList"
  , testRoundTripProp' @(V.Vector Int)       "V.Vector"
  , testRoundTripProp' @(VS.Vector Int)      "VS.Vector"
  , testRoundTripProp' @(VP.Vector Int)      "VP.Vector"
  , testRoundTripProp' @(VU.Vector Int)      "VU.Vector"
  , testRoundTripProp' @(Set Int)            "Set"
  , testRoundTripProp' @(Map T.Text Int)     "Map"
  , testRoundTripProp' @(HashSet Int)        "HashSet"
  , testRoundTripProp' @(HashMap T.Text Int) "HashMap"
  , testRoundTripProp' @(Int, Bool)                        "Tuple2"
  , testRoundTripProp' @(Int, Bool, T.Text)                "Tuple3"
  , testRoundTripProp' @(Int, Bool, T.Text, [Int])         "Tuple4"
  , testRoundTripProp' @(Int, Bool, T.Text, [Int], Double) "Tuple5"
  ]
