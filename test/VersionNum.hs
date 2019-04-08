{-# LANGUAGE ScopedTypeVariables #-}
module VersionNum where


import Data.Proxy
import Test.Tasty as Tasty
import Test.Tasty.QuickCheck as Tasty


numTest :: (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
numTest p = testGroup "Version's Num instance" $
    ($p) <$> [plusTest, minusTest, multTest, negateTest, absSignumTest]

plusTest :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
plusTest _ = testGroup "Plus laws"
    [ plusAssociative
    , plusCommutative
    , plusIdentity
    ]
  where plusAssociative = testProperty "Associative (+)" go
          where go a b c = (a :: a) + (b + c) == (a + b) + c
        plusCommutative = testProperty "Commutative (+)" go
          where go a b = (a :: a) + b == b + a
        plusIdentity = testProperty "Identity (+)" go
          where go a = (a :: a) + fromInteger 0 == a

minusTest :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
minusTest _ = testProperty "Minus law" $ \a -> (a :: a) - a == fromInteger 0

multTest :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
multTest _ = testGroup "Plus laws"
    [ multAssociative
    , multCommutative
    , multIdentity
    ]
  where multAssociative = testProperty "Associative (*)" go
          where go a b c = (a :: a) * (b * c) == (a * b) * c
        multCommutative = testProperty "Commutative (*)" go
          where go a b = (a :: a) * b == b * a
        multIdentity = testProperty "Identity (*)" go
          where go a = (a :: a) * fromInteger 1 == a

negateTest :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
negateTest _ = testGroup "Negate laws"
    [ negateToZero
    , doubleNegate
    ]
  where negateToZero = testProperty "Self added to negated self == identity" $
                            \a -> (a :: a) + negate a == fromInteger 0
        doubleNegate = testProperty "Double negation is original" $
                            \a -> negate (negate a) == (a :: a)

absSignumTest :: forall a. (Num a, Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
absSignumTest _ = testProperty "Absolute time signum is original" $
                      \a -> abs a * signum a == (a :: a)
