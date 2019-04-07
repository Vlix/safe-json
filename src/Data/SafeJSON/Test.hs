{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.SafeJSON.Test (
  testConsistency
  , Proxy(..)
  , testConsistency'
  , testSafeToFrom
  , testMigration
  , testReverseMigration
  , (>=?)
  , (<=?)
  ) where


import Data.Aeson.Types (parseEither)
import Data.Proxy
import Data.SafeJSON.Internal
import Test.Tasty.HUnit (Assertion, assertEqual)


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

-- | Similar to 'testMigration', but using 'Migrate (Reverse a)'.
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
