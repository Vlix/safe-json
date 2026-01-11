{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Instances (
    DotNetTime()
  , DList()
  , VP.Vector()
  , Value()
  ) where


import Data.Aeson
#if MIN_VERSION_aeson(2,0,0) && !MIN_VERSION_aeson(2,0,3)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
#endif
import Data.DList as DList (DList, fromList, toList)
import Data.Int (Int64)
#if MIN_VERSION_quickcheck_instances(0,4,0)
import Data.List.NonEmpty as NonEmpty (NonEmpty, fromList, toList)
import Data.Semigroup as Semigroup (First (..), Last (..), Max (..), Min (..))
#endif
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector.Primitive as VP
#if MIN_VERSION_quickcheck_instances(0,4,0)
import Numeric.Natural (Natural)
#endif

import Test.Tasty.QuickCheck (
  Arbitrary (..),
#if MIN_VERSION_quickcheck_instances(0,4,0)
  Arbitrary1 (..),
  arbitrary1,
  arbitrarySizedNatural,
  listOf1,
  shrink1,
  shrinkIntegral,
#endif
 )
#if !MIN_VERSION_aeson(2,0,3)
import Test.Tasty.QuickCheck (oneof, resize)
import Test.QuickCheck.Arbitrary.Generic (genericShrink)
#endif
import Test.QuickCheck.Instances()

instance Arbitrary DotNetTime where
  arbitrary = do
      diff <- arbitrary
      -- DotNetTime is only accurate to the millisecond
      let floored = (/ 1000)
                  . fromIntegral
                  . (floor :: NominalDiffTime -> Int64)
                  $ (diff :: NominalDiffTime) * 1000
      return $ DotNetTime $ posixSecondsToUTCTime floored
  shrink = fmap DotNetTime . shrink . fromDotNetTime

instance Arbitrary a => Arbitrary (DList a) where
  arbitrary = DList.fromList <$> arbitrary
  shrink = fmap DList.fromList . shrink . DList.toList

#if !MIN_VERSION_quickcheck_instances(0,3,32)
instance (Arbitrary a, VP.Prim a) => Arbitrary (VP.Vector a) where
  arbitrary = VP.fromList <$> arbitrary
  shrink = fmap VP.fromList . shrink . VP.toList
#endif

#if MIN_VERSION_aeson(2,0,0) && !MIN_VERSION_aeson(2,0,3)
instance Arbitrary v => Arbitrary (KM.KeyMap v) where
    arbitrary = KM.fromList <$> arbitrary

instance Arbitrary K.Key where
    arbitrary = K.fromText <$> arbitrary
#endif

#if !MIN_VERSION_aeson(2,0,3)
instance Arbitrary Value where
  arbitrary = oneof
    [ resize 5 $ Object <$> arbitrary
    , resize 5 $ Array <$> arbitrary
    , String <$> arbitrary
    , Number <$> arbitrary
    , Bool <$> arbitrary
    , pure Null
    ]
  shrink = genericShrink
#endif

#if !MIN_VERSION_aeson(1,5,2)
-- | This is here just to test 'Set' in 'parseCollection'
instance Ord Value where
  Null `compare` Null = EQ
  Null `compare` _    = LT
  _    `compare` Null = GT
  a `compare` b
    | Bool   a' <- a, Bool   b' <- b = a' `compare` b'
    | Number a' <- a, Number b' <- b = a' `compare` b'
    | String a' <- a, String b' <- b = a' `compare` b'
    | Array  a' <- a, Array  b' <- b = a' `compare` b'
    | Object a' <- a, Object b' <- b = a' `compare` b'
  Bool{}   `compare` _      = LT
  Number{} `compare` Bool{} = GT
  Number{} `compare` _      = LT
  String{} `compare` Bool{}   = GT
  String{} `compare` Number{} = GT
  String{} `compare` _        = LT
  Array{}  `compare` Object{} = LT
  _        `compare` _        = GT
#endif

#if MIN_VERSION_quickcheck_instances(0,4,0)
instance Arbitrary1 NonEmpty where
  liftArbitrary arb = NonEmpty.fromList <$> listOf1 arb
  liftShrink shr xs = [ NonEmpty.fromList xs' | xs' <- liftShrink shr (NonEmpty.toList xs), not (null xs') ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary1
  shrink = shrink1

instance Arbitrary a => Arbitrary (Semigroup.Min a) where
  arbitrary = fmap Semigroup.Min arbitrary
  shrink = map Semigroup.Min . shrink . Semigroup.getMin

instance Arbitrary a => Arbitrary (Semigroup.Max a) where
  arbitrary = fmap Semigroup.Max arbitrary
  shrink = map Semigroup.Max . shrink . Semigroup.getMax

instance Arbitrary a => Arbitrary (Semigroup.First a) where
  arbitrary = fmap Semigroup.First arbitrary
  shrink = map Semigroup.First . shrink . Semigroup.getFirst

instance Arbitrary a => Arbitrary (Semigroup.Last a) where
  arbitrary = fmap Semigroup.Last arbitrary
  shrink = map Semigroup.Last . shrink . Semigroup.getLast

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral
#endif
