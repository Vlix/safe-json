{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Data.SafeJSON.Test
Copyright   : (c) 2019 Felix Paulusma
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : experimental

This module contains some functions to use for testing
'SafeJSON' and 'Migrate' instances.
-}
module Data.SafeJSON.Test (
  -- * Consistency checks
  --
  -- | It is advised to always run 'testConsistency' (or
  --   'testConsistency'') for all your types that have
  --   a 'SafeJSON' instance.
  --
  --   __Note that any type that fails this test will also__
  --   __fail any 'safeFromJSON' parsing!__

  -- ** Using TypeApplications
    testConsistency
  -- ** Using a Proxy argument
  , testConsistency'
  -- * Unit tests
  --
  -- ** Migration tests
  --
  -- | These tests can be used to verify the implemented
  --   'migrate' function acts as expected.
  , testMigration
  , testReverseMigration
  -- *** Synonyms
  , (<=?)
  , (>=?)
  -- ** Round trip tests
  --
  -- | These tests can be used to verify that round trips are
  --   consistent. Either directly ('testRoundTrip'), through
  --   a forward migration ('migrateRoundTrip') or a reversed
  --   backward migration ('migrateReverseRoundTrip').
  , testRoundTrip
  , migrateRoundTrip
  , migrateReverseRoundTrip
  -- * Property tests
  --
  -- | Useful if your types also have 'Arbitrary' instances.

  -- *** Constraint synonyms for readability
  --
  , TestMigrate
  , TestReverseMigrate
  -- ** Using TypeApplications
  , testRoundTripProp
  , migrateRoundTripProp
  , migrateReverseRoundTripProp
  -- ** Using a Proxy argument
  , testRoundTripProp'
  , migrateRoundTripProp'
  , migrateReverseRoundTripProp'
  -- * Re-export for convenience
  , Proxy(..)
  ) where


import Data.Aeson.Types (parseEither)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Proxy
import Data.SafeJSON.Internal
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)


-- | Useful in test suites. Will fail if anything in the
--   chain of your types is inconsistent.
--
--   === __Example usage:__
--
-- > testConsistency @MyType
testConsistency :: forall a. SafeJSON a => Assertion
testConsistency = checkConsistency p $ \_ -> return ()
  where p = Proxy :: Proxy a

-- | Useful in test suites. Will fail if anything in the
--   chain of your types is inconsistent.
testConsistency' :: forall a. SafeJSON a => Proxy a -> Assertion
testConsistency' = flip checkConsistency $ \_ -> return ()

-- | Tests that the following holds:
--
--   prop> Just a == parseMaybe safeFromJSON (safeToJSON a)
testRoundTrip :: forall a. (Show a, Eq a, SafeJSON a) => a -> Assertion
testRoundTrip a = (typeName (Proxy :: Proxy a) <> ": to JSON and back not consistent") `assertEqual` Right a $
    parseEither (safeFromJSON . safeToJSON) a

-- | Tests that the following holds __for all @a@__:
--
--   prop> Just a == parseMaybe safeFromJSON (safeToJSON a)
testRoundTripProp' :: forall a. (Eq a, Show a, Arbitrary a, SafeJSON a) => Proxy a -> String -> TestTree
testRoundTripProp' _ s = testProperty s $ \a ->
    Right (a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | Tests that the following holds for all @a@:
--
--   prop> Just a == parseMaybe safeFromJSON (safeToJSON a)
--
--   === __Example usage:__
--
-- > testRoundTripProp @MyType s
testRoundTripProp :: forall a. (Eq a, Show a, Arbitrary a, SafeJSON a) => String -> TestTree
testRoundTripProp s = testProperty s $ \a ->
    Right (a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | Migration test. Mostly useful as regression test.
--
--   First argument is the older type which should turn into
--   the second argument after migrating using 'migrate'.
--
--   prop> Just (migrate a) == parseMaybe safeFromJSON (safeToJSON a)
testMigration :: (Show a, Eq a, Migrate a) => MigrateFrom a -> a -> Assertion
testMigration = assertEqual "Unexpected result of SafeJSON migration" . migrate

-- | Similar to 'testMigration', but using @Migrate (Reverse a)@.
--
--   The first argument here is the newer type, which will be migrated back
--   to the expected second argument (older type).
--
--   prop> Just (unReverse $ migrate a) == parseMaybe safeFromJSON (safeToJSON a)
testReverseMigration :: (Show a, Eq a, Migrate (Reverse a)) => MigrateFrom (Reverse a) -> a -> Assertion
testReverseMigration = assertEqual "Unexpected result of SafeJSON migration" . unReverse . migrate

infix 1 >=?, <=?
-- | Operator synonymous with 'testMigration'
(<=?) :: (Show a, Eq a, Migrate a) => MigrateFrom a -> a -> Assertion
(<=?) = testMigration

-- | Operator synonymous with 'testReverseMigration'
(>=?) :: (Show a, Eq a, Migrate (Reverse a)) => MigrateFrom (Reverse a) -> a -> Assertion
(>=?) = testReverseMigration

-- | This test verifies that direct migration, and migration
--   through encoding and decoding to the newer type, is equivalent.
migrateRoundTrip :: forall a. (Eq a, Show a, SafeJSON a, Migrate a) => MigrateFrom a -> Assertion
migrateRoundTrip oldType = "Unexpected result of decoding encoded older type" `assertEqual` Right (migrate oldType :: a) $
    parseEither (safeFromJSON . safeToJSON) oldType

-- | Similar to 'migrateRoundTrip', but tests the migration from a newer type
--   to the older type, in case of a @Migrate (Reverse a)@ instance
migrateReverseRoundTrip :: forall a. (Eq a, Show a, SafeJSON a, Migrate (Reverse a)) => MigrateFrom (Reverse a) -> Assertion
migrateReverseRoundTrip newType = "Unexpected result of decoding encoded newer type" `assertEqual` Right (unReverse $ migrate newType :: a) $
    parseEither (safeFromJSON . safeToJSON) newType

-- | Constraints for migrating from a previous version
type TestMigrate a b =
    ( Eq a
    , Show (MigrateFrom a)
    , Arbitrary (MigrateFrom a)
    , SafeJSON a
    , SafeJSON (MigrateFrom a)
    , Migrate a
    , MigrateFrom a ~ b
    )

-- | This test verifies that direct migration, and migration
--   through encoding and decoding to the newer type, is equivalent
--   __for all @a@__.
--
--   prop> Just (migrate a) == parseMaybe safeFromJSON (safeToJSON a)
migrateRoundTripProp' :: forall a b. TestMigrate a b => Proxy (a,b) -> String -> TestTree
migrateRoundTripProp' _ s = testProperty s $ \a ->
    Right (migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | This test verifies that direct migration, and migration
--   through encoding and decoding to the newer type, is equivalent
--   __for all @a@__.
--
--   prop> Just (migrate a) == parseMaybe safeFromJSON (safeToJSON a)
--
--   === __Example usage:__
--
-- > migrateRoundTripProp @NewType @OldType s
migrateRoundTripProp :: forall a b. TestMigrate a b => String -> TestTree
migrateRoundTripProp s = testProperty s $ \a ->
    Right (migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | Constraints for migrating from a future version
type TestReverseMigrate a b =
    ( Eq a
    , Show (MigrateFrom (Reverse a))
    , Arbitrary (MigrateFrom (Reverse a))
    , SafeJSON a
    , Migrate (Reverse a)
    , MigrateFrom (Reverse a) ~ b
    )

-- | Similar to 'migrateRoundTripProp, but tests the migration from a newer type
--   to the older type, in case of a @Migrate (Reverse a)@ instance.
--
--   prop> Just (unReverse $ migrate a) == parseMaybe safeFromJSON (safeToJSON a)
migrateReverseRoundTripProp' :: forall a b. TestReverseMigrate a b => Proxy (a,b) -> String -> TestTree
migrateReverseRoundTripProp' _ s = testProperty s $ \a ->
    Right (unReverse $ migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | Similar to 'migrateRoundTripProp', but tests the migration from a newer type
--   to the older type, in case of a @Migrate (Reverse a)@ instance.
--
--   prop> Just (unReverse $ migrate a) == parseMaybe safeFromJSON (safeToJSON a)
--
--   === __Example usage:__
--
--   /Please also note the reversing of the type applications./
--
-- > migrateReverseRoundTripProp @OldType @NewType s
migrateReverseRoundTripProp :: forall a b. TestReverseMigrate a b => String -> TestTree
migrateReverseRoundTripProp s = testProperty s $ \a ->
    Right (unReverse $ migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a
