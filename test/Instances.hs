{-# OPTIONS_GHC -Wno-orphans #-}
module Instances (
  module Test.QuickCheck.Instances
  , DotNetTime()
  , DList()
  , VP.Vector()
  , Value()
  ) where


import Data.Aeson
import Data.DList (DList, fromList, toList)
import Data.Int (Int64)
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector.Primitive as VP

import Test.Tasty.QuickCheck (Arbitrary(..), oneof, resize)
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances()

-- instance Eq ZonedTime where
  -- zt1 == zt2 =
      -- zonedTimeToLocalTime zt1 == zonedTimeToLocalTime zt2
      -- && zonedTimeZone zt1 == zonedTimeZone zt2

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
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . shrink . toList

instance (Arbitrary a, VP.Prim a) => Arbitrary (VP.Vector a) where
  arbitrary = VP.fromList <$> arbitrary
  shrink = fmap VP.fromList . shrink . VP.toList

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
