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
  [ {-primitiveConsistency
  ,-} catchLoops
  , catchBadInstance
  , catchBadChain
  , catchBadVersion
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
  kind = extended_extension

instance SafeJSON LoopType2 where
  version = 1
  kind = extended_extension

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
  migrate = const LoopType1

instance Migrate LoopType2 where
  type MigrateFrom LoopType2 = LoopType1
  migrate = const LoopType2

instance Migrate (Reverse LoopType1) where
  type MigrateFrom (Reverse LoopType1) = LoopType2
  migrate = const $ Reverse LoopType1

instance Migrate (Reverse LoopType2) where
  type MigrateFrom (Reverse LoopType2) = LoopType1
  migrate = const $ Reverse LoopType2


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

catchBadChain :: TestTree
catchBadChain = shouldFail
    "Catch bad SafeJSON instances (duplicate)"
    "Allowed instances with duplicate versions"
    $ do
      shouldNotFail "DuplicateType is consistent" $ testConsistency' @DuplicateType
      testConsistency' @DuplicateType1
  where shouldNotFail :: String -> IO () -> IO ()
        shouldNotFail s io = io `catch` go
          where go :: SomeException -> IO ()
                go e = assertFailure $ s ++ ": " ++ show e

catchBadVersion :: TestTree
catchBadVersion = shouldFail
    "Catch bad SafeJSON instance (noVersion)"
    "Allowed 'noVersion' with non-(extended_)base 'kind'"
    $ testConsistency' @DuplicateType2

data DuplicateType = DuplicateType Text

data DuplicateType1 = DuplicateType1 Text

data DuplicateType2 = DuplicateType2 Text

instance FromJSON DuplicateType where
  parseJSON = withText "DuplicateType" $ pure . DuplicateType

instance ToJSON DuplicateType where
  toJSON (DuplicateType t) = String t

instance SafeJSON DuplicateType where
  version = 1
  kind = base

instance FromJSON DuplicateType1 where
  parseJSON = withText "DuplicateType1" $ pure . DuplicateType1

instance ToJSON DuplicateType1 where
  toJSON (DuplicateType1 t) = String t

instance SafeJSON DuplicateType1 where
  version = 1
  kind = extension

instance Migrate DuplicateType1 where
  type MigrateFrom DuplicateType1 = DuplicateType
  migrate (DuplicateType t) = DuplicateType1 t


instance FromJSON DuplicateType2 where
  parseJSON = withText "DuplicateType2" $ pure . DuplicateType2

instance ToJSON DuplicateType2 where
  toJSON (DuplicateType2 t) = String t

instance SafeJSON DuplicateType2 where
  version = noVersion
  kind = extension

instance Migrate DuplicateType2 where
  type MigrateFrom DuplicateType2 = DuplicateType1
  migrate (DuplicateType1 t) = DuplicateType2 t
