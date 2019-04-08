{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.SafeJSON.Test (
  testConsistency
  , Proxy(..)
  , testConsistency'
  , testSafeToFrom
  , testMigration
  , testReverseMigration
  , (>=?)
  , (<=?)
  , testRoundTrip
  , testReverseRoundTrip
  -- * For testing purposes
  , Version()
  ) where


import Data.Aeson.Types (parseEither)
import Data.Proxy
import Data.SafeJSON.Internal
import Test.Tasty.HUnit (Assertion, assertEqual)
import Test.Tasty.QuickCheck (Arbitrary(..), shrinkIntegral)


-- | /(without @TypeApplication@ pragma)/
--   Useful in test suites. Will fail if anything in the
--   chain of your types is inconsistent.
--
--   Example usage:
--
--   @testConsistency (Proxy :: Proxy MyType)@
--
--   @testConsistency \@MyType Proxy@
testConsistency :: forall a. SafeJSON a => Proxy a -> Assertion
testConsistency = flip checkConsistency $ return ()

-- | /(with @TypeApplication@ pragma)/
--   Useful in test suites. Will fail if anything in the
--   chain of your types is inconsistent.
--
--   Example usage:
--
--   @testConsistency \@MyType@
testConsistency' :: forall a. SafeJSON a => Assertion
testConsistency' = checkConsistency p $ return ()
  where p = Proxy :: Proxy a

-- | Basically a @ToJSON <-> FromJSON@ test if the standard
--   definitions of 'safeTo' and 'safeFrom' haven't been changed.
testSafeToFrom :: (Show a, Eq a, SafeJSON a) => a -> Assertion
testSafeToFrom a = "To JSON and back not consistent" `assertEqual` Right a $
    parseEither (safeFromJSON . safeToJSON) a

-- | Migration test. Mostly useful as regression test.
--
-- First argument is the older type which should turn into
-- the second argument after migrating using 'migrate'.
testMigration :: (Show a, Eq a, Migrate a) => MigrateFrom a -> a -> Assertion
testMigration = assertEqual "Unexpected result of SafeJSON migration" . migrate

-- | Similar to 'testMigration', but using @Migrate (Reverse a)@.
--
-- The first argument here is the newer type, which will be migrated back
-- to the expected second argument (older type).
testReverseMigration :: (Show a, Eq a, Migrate (Reverse a)) => MigrateFrom (Reverse a) -> a -> Assertion
testReverseMigration = assertEqual "Unexpected result of SafeJSON migration" . unReverse . migrate

infix 1 >=?, <=?
-- | Operator synonymous with 'testMigration'
(>=?) :: (Show a, Eq a, Migrate a) => MigrateFrom a -> a -> Assertion
(>=?) = testMigration

-- | Operator synonymous with 'testReverseMigration'
(<=?) :: (Show a, Eq a, Migrate (Reverse a)) => MigrateFrom (Reverse a) -> a -> Assertion
(<=?) = testReverseMigration

-- | This test verifies that direct migration, and migration
--   through encoding and decoding to the newer type, is equivalent.
testRoundTrip :: forall a. (Eq a, Show a, SafeJSON a, Migrate a) => MigrateFrom a -> Assertion
testRoundTrip oldType = "Unexpected result of decoding encoded older type" `assertEqual` Right (migrate oldType :: a) $
    parseEither (safeFromJSON . safeToJSON) oldType

-- | Similar to 'testRoundTrip', but tests the migration from a newer type
--   to the older type, in case of a @Migrate (Reverse a)@ instance
testReverseRoundTrip :: forall a. (Eq a, Show a, SafeJSON a, Migrate (Reverse a)) => MigrateFrom (Reverse a) -> Assertion
testReverseRoundTrip newType = "Unexpected result of decoding encoded newer type" `assertEqual` Right (unReverse $ migrate newType :: a) $
    parseEither (safeFromJSON . safeToJSON) newType

instance Arbitrary (Version a) where
  arbitrary = Version . Just <$> arbitrary
  shrink (Version Nothing) = []
  shrink (Version (Just a)) = noVersion : (Version . Just <$> shrinkIntegral a)
