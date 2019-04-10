{-# LANGUAGE AllowAmbiguousTypes #-}
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
'SafeJSON' instances.
-}
module Data.SafeJSON.Test (
  -- * Consistency checks
  --
  -- It is advised to always run one of the @testConsistency@
  -- tests for all your types that have 'SafeJSON' instances.
  -- Note that any type that fails this test will also fail
  -- any 'safeFromJSON' parsing!
    testConsistency
  , testConsistency'
  , Proxy(..)
  -- * Unit tests
  --
  -- ** Migration tests
  --
  -- These tests can be used to verify the implemented
  -- 'migrate' function acts as expected.
  , testMigration
  , testReverseMigration
  -- *** Synonyms
  , (>=?)
  , (<=?)
  -- ** Round trip tests
  --
  -- These tests can be used to verify that round trips are
  -- consistent. Either directly ('testRoundTrip'), through
  -- a forward migration (migrateRoundTrip) or a reversed
  -- backward migration (migrateReverseRoundTrip).
  , testRoundTrip
  , migrateRoundTrip
  , migrateReverseRoundTrip
  -- * Property tests
  --
  -- Useful if your types also have 'Arbitrary' instances.
  , testRoundTripProp
  , testRoundTripProp'
  , migrateRoundTripProp
  , migrateRoundTripProp'
  , migrateReverseRoundTripProp
  , migrateReverseRoundTripProp'
  ) where


import Data.Aeson.Types (parseEither)
import Data.Proxy
import Data.SafeJSON.Internal
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertEqual)
import Test.Tasty.QuickCheck (Arbitrary, testProperty)


-- | /(without @TypeApplication@ pragma)/
--   Useful in test suites. Will fail if anything in the
--   chain of your types is inconsistent.
--
--   Example usage:
--
--   @testConsistency (Proxy :: Proxy MyType)@
testConsistency :: forall a. SafeJSON a => Proxy a -> Assertion
testConsistency = flip checkConsistency $ \_ -> return ()

-- | /(with @TypeApplication@ pragma)/
--   Useful in test suites. Will fail if anything in the
--   chain of your types is inconsistent.
--
--   Example usage:
--
--   @testConsistency \@MyType@
testConsistency' :: forall a. SafeJSON a => Assertion
testConsistency' = checkConsistency p $ \_ -> return ()
  where p = Proxy :: Proxy a

-- | Tests that the following holds:
--
--   @a == (safeFromJSON . safeToJSON) a
testRoundTrip :: (Show a, Eq a, SafeJSON a) => a -> Assertion
testRoundTrip a = "To JSON and back not consistent" `assertEqual` Right a $
    parseEither (safeFromJSON . safeToJSON) a

-- | /(without @TypeApplication@ pragma)/
--   Tests that the following holds for all @a@:
--
--   @testProperty s $ \a -> a == (safeFromJSON . safeToJSON) a@
--
--   Example usage:
--
--   @testRoundTripProp (Proxy :: Proxy MyType) s@
testRoundTripProp :: forall a. (Eq a, Show a, Arbitrary a, SafeJSON a) => Proxy a -> String -> TestTree
testRoundTripProp _ s = testProperty s $ \a ->
    Right (a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | /(with @TypeApplication@ pragma)/
--   Tests that the following holds for all @a@:
--
--   @testProperty s $ \a -> a == (safeFromJSON . safeToJSON) a@
--
--   Example usage:
--
--   @testRoundTripProp' @MyType s@
testRoundTripProp' :: forall a. (Eq a, Show a, Arbitrary a, SafeJSON a) => String -> TestTree
testRoundTripProp' s = testProperty s $ \a ->
    Right (a :: a) == parseEither (safeFromJSON . safeToJSON) a

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
migrateRoundTrip :: forall a. (Eq a, Show a, SafeJSON a, Migrate a) => MigrateFrom a -> Assertion
migrateRoundTrip oldType = "Unexpected result of decoding encoded older type" `assertEqual` Right (migrate oldType :: a) $
    parseEither (safeFromJSON . safeToJSON) oldType

-- | Similar to 'migrateRoundTrip', but tests the migration from a newer type
--   to the older type, in case of a @Migrate (Reverse a)@ instance
migrateReverseRoundTrip :: forall a. (Eq a, Show a, SafeJSON a, Migrate (Reverse a)) => MigrateFrom (Reverse a) -> Assertion
migrateReverseRoundTrip newType = "Unexpected result of decoding encoded newer type" `assertEqual` Right (unReverse $ migrate newType :: a) $
    parseEither (safeFromJSON . safeToJSON) newType

-- | /(without @TypeApplication@ pragma)/
--   This test verifies that direct migration, and migration
--   through encoding and decoding to the newer type, is equivalent
--   for all @a@.
--
--   Example usage:
--
--   @migrateRoundTripProp (Proxy :: Proxy (NewType,OldType)) s@
migrateRoundTripProp :: forall a b.
                        ( Eq a
                        , Show (MigrateFrom a)
                        , Arbitrary (MigrateFrom a)
                        , SafeJSON a
                        , SafeJSON (MigrateFrom a)
                        , Migrate a
                        , MigrateFrom a ~ b
                        )
                     => Proxy (a,b) -> String -> TestTree
migrateRoundTripProp _ s = testProperty s $ \a ->
    Right (migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | /(with @TypeApplication@ pragma)/
--   This test verifies that direct migration, and migration
--   through encoding and decoding to the newer type, is equivalent
--   for all @a@.
--
--   Example usage:
--
--   @migrateRoundTripProp' @NewType @OldType s@
migrateRoundTripProp' :: forall a b.
                         ( Eq a
                         , Show (MigrateFrom a)
                         , Arbitrary (MigrateFrom a)
                         , SafeJSON a
                         , SafeJSON (MigrateFrom a)
                         , Migrate a
                         , MigrateFrom a ~ b
                         )
                      => String -> TestTree
migrateRoundTripProp' s = testProperty s $ \a ->
    Right (migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | /(with @TypeApplication@ pragma)/
--   Similar to 'migrateRoundTripProp', but tests the migration from a newer type
--   to the older type, in case of a @Migrate (Reverse a)@ instance.
--   Please also note the reversing of the type applications.
--
--   Example usage:
--
--   @migrateReverseRoundTripProp (Proxy :: Proxy (OldType,NewType)) s@
migrateReverseRoundTripProp :: forall a b.
                               ( Eq a
                               , Show (MigrateFrom (Reverse a))
                               , Arbitrary (MigrateFrom (Reverse a))
                               , SafeJSON a
                               , Migrate (Reverse a)
                               , MigrateFrom (Reverse a) ~ b
                               )
                            => Proxy (a,b) -> String -> TestTree
migrateReverseRoundTripProp _ s = testProperty s $ \a ->
    Right (unReverse $ migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a

-- | /(with @TypeApplication@ pragma)/
--   Similar to 'migrateRoundTripProp'', but tests the migration from a newer type
--   to the older type, in case of a @Migrate (Reverse a)@ instance.
--   Please also note the reversing of the type applications.
--
--   Example usage:
--
--   @migrateReverseRoundTripProp' @OldType @NewType s@
migrateReverseRoundTripProp' :: forall a b.
                                ( Eq a
                                , Show (MigrateFrom (Reverse a))
                                , Arbitrary (MigrateFrom (Reverse a))
                                , SafeJSON a
                                , Migrate (Reverse a)
                                , MigrateFrom (Reverse a) ~ b
                                )
                             => String -> TestTree
migrateReverseRoundTripProp' s = testProperty s $ \a ->
    Right (unReverse $ migrate a :: a) == parseEither (safeFromJSON . safeToJSON) a
