{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module ConsistencyTests where


import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import Consistency.Primitives (primitiveConsistency)
import Consistency.Migrations (migrationConsistency)
import Data.SafeJSON
import Data.SafeJSON.Test (testConsistency', testRoundTrip)


-- FIXME: Better testing would be possible with specific Exceptions
-- raised by testConsistency. (Or 'invalidChain' should give custom
-- types instead of String, so mkProfile/computeConsistency can output
-- a result to match on)

consistencyTests :: TestTree
consistencyTests = testGroup "Consistency"
  [ catchLoops
  , catchBadInstance
  , dontCatchGoodType
  , catchBadKind
  , catchBadKind2
  , catchBadKind3
  , catchBadKind4
  , catchBadKind5
  , catchBadKind6
  , primitiveConsistency
  , migrationConsistency
  ]


shouldFail :: String -> String -> IO () -> TestTree
shouldFail s err io = testCase s $ do
    b <- tryIt `catch` success
    when b $ assertFailure err
  where tryIt = io >> return True
        success :: SomeException -> IO Bool
        success _ = return False


-----------------------
-- LOOPING INSTANCES --
-----------------------

-- | This is important, since if this wouldn't be caught
-- any invocation of 'checkConsistency' (like in safeFromJSON)
-- will lock up and eat up all your memory.
catchLoops :: TestTree
catchLoops = shouldFail
    "Catch looping instances (checkConsistency)"
    "Didn't catch looping instance"
    $ testConsistency' @LoopType1


data LoopType1 = LoopType1 deriving (Eq, Show)

data LoopType2 = LoopType2

instance SafeJSON LoopType1 where
  version = 0
  kind = extension

instance SafeJSON LoopType2 where
  version = 1
  kind = extension

instance ToJSON LoopType1 where
  toJSON _ = Null

instance ToJSON LoopType2 where
  toJSON _ = Null

instance FromJSON LoopType1 where
  parseJSON Null = pure LoopType1
  parseJSON _ = fail "uhhh wat"

instance FromJSON LoopType2 where
  parseJSON Null = pure LoopType2
  parseJSON _ = fail "uhhh wat"

instance Migrate LoopType1 where
  type MigrateFrom LoopType1 = LoopType2
  migrate _ = LoopType1

instance Migrate LoopType2 where
  type MigrateFrom LoopType2 = LoopType1
  migrate _ = LoopType2


-------------------------
-- Catch bad instances --
-------------------------

-- | Just for redundancy
catchBadInstance :: TestTree
catchBadInstance = shouldFail
    "Catch bad JSON instance"
    "Allowed bad instance conversion"
    $ testRoundTrip $ BadJSON 2


data BadJSON = BadJSON Int deriving (Eq, Show)

instance FromJSON BadJSON where
  parseJSON = withText "BadJSON" $ \case
    "bad" -> pure $ BadJSON 1
    _ -> fail "wat"

instance ToJSON BadJSON where
  toJSON (BadJSON 2) = String "bad"
  toJSON _ = String "wat"

instance SafeJSON BadJSON where
  version = noVersion


---------------------
-- Catch bad chain --
---------------------

dontCatchGoodType :: TestTree
dontCatchGoodType = testCase "DuplicateType is consistent" $
    testConsistency' @DuplicateType

dontCatchGoodType2 :: TestTree
dontCatchGoodType2 = testCase "DuplicateType0 is consistent" $
    testConsistency' @DuplicateType0

catchBadKind :: TestTree
catchBadKind = shouldFail
    "Catch bad SafeJSON instances (duplicate version)"
    "Allowed instances with duplicate versions"
    $ testConsistency' @DuplicateType1

catchBadKind2 :: TestTree
catchBadKind2 = shouldFail
    "Catch bad SafeJSON instance (noVersion + extension)"
    "Allowed 'noVersion' with non-(extended_)base 'kind'"
    $ testConsistency' @DuplicateType2

catchBadKind3 :: TestTree
catchBadKind3 = shouldFail
    "Catch bad SafeJSON instance (noVersion + extended_extension)"
    "Allowed 'noVersion' with non-(extended_)base 'kind'"
    $ testConsistency' @DuplicateType3

catchBadKind4 :: TestTree
catchBadKind4 = shouldFail
    "Catch bad SafeJSON instance (duplicate future version)"
    "Allowed future type with same version"
    $ testConsistency' @DuplicateType4

-- Kind of redundant because of 'catchBadKind', but hey...
catchBadKind5 :: TestTree
catchBadKind5 = shouldFail
    "Catch bad SafeJSON instance (duplicate past version)"
    "Allowed past type with same version"
    $ testConsistency' @DuplicateType5

catchBadKind6 :: TestTree
catchBadKind6 = shouldFail
    "Catch bad SafeJSON instance (duplicate versions in chain)"
    "Allowed past types with same version (this type's version not source of collision)"
    $ testConsistency' @DuplicateType6

--------------------------------------------------------------
-- Conflicting version numbering / bad kinds
--------------------------------------------------------------

#define DUPLICATE(TYPE,VERSION,KIND)             \
data TYPE = TYPE Text;                           \
instance FromJSON TYPE where {                   \
    parseJSON = withText "TYPE" $ pure . TYPE }; \
instance ToJSON TYPE where {                     \
    toJSON (TYPE t) = String t };                \
instance SafeJSON TYPE where {                   \
  version = VERSION; kind = KIND }

#define MIGRATE(TYPE,OLDTYPE)      \
instance Migrate TYPE where {      \
  type MigrateFrom TYPE = OLDTYPE; \
  migrate (OLDTYPE t) = TYPE t }

#define REVERSE(TYPE,NEWTYPE)      \
instance Migrate (Reverse TYPE) where {      \
  type MigrateFrom (Reverse TYPE) = NEWTYPE; \
  migrate (NEWTYPE t) = Reverse $ TYPE t }

-- Basic type/instance, consistent
DUPLICATE(DuplicateType,noVersion,base)

-- Basic type/instance, consistent
DUPLICATE(DuplicateType0,1,base)

-- Extending type/instance, inconsistent (duplicate version number)
DUPLICATE(DuplicateType1,1,extension)
MIGRATE(DuplicateType1,DuplicateType0)

-- Extending type/instance, inconsistent (noVersion + extension)
DUPLICATE(DuplicateType2,noVersion,extension)
MIGRATE(DuplicateType2,DuplicateType0)

-- Extending type/instance, inconsistent (noVersion + extended_extension)
DUPLICATE(DuplicateType3,noVersion,extended_extension)
MIGRATE(DuplicateType3,DuplicateType0)
REVERSE(DuplicateType3,DummyDuplicate)

-- This is just here so DuplicateType3 has something to 'MigrateFrom (Reverse a)'
DUPLICATE(DummyDuplicate,2,extension)
MIGRATE(DummyDuplicate,DuplicateType3)

-- Extended type/instance, inconsistent (future type has duplicate version number)
DUPLICATE(DuplicateType4,9,extended_base)
REVERSE(DuplicateType4,DuplicateType5)

-- Extended type/instance, inconsistent (past type has duplicate version number)
DUPLICATE(DuplicateType5,9,extended_extension)
MIGRATE(DuplicateType5,DuplicateType4)
REVERSE(DuplicateType5,DuplicateType6)

-- Extending type/instance, inconsistent (older types have duplicate version numbers)
DUPLICATE(DuplicateType6,10,extension)
MIGRATE(DuplicateType6,DuplicateType5)
