{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Version where


import Data.Aeson (Value, (.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseEither)
-- import Data.Proxy
import Data.Text as T
import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty

import Data.SafeJSON


versionFuncTests :: TestTree
versionFuncTests = testGroup "Version functions"
  [ setTest
  , removeTest
  ]

setTest :: TestTree
setTest = testGroup "Set version" []

removeTest :: TestTree
removeTest = testGroup "Remove version" []


-- | Given a field in the "version.json" object, parses as
-- the given type, but hardsets version before doing so.
parseSetVersion :: forall a. (Eq a, Show a, SafeJSON a) => Text -> a -> TestTree
parseSetVersion t val = parseAnd t go (setVersion_ @a)
  where go = assertEqual "Unexpected result" val

-- | Given a field in the "version.json" object, tries to
-- compare the plain JSON with the (removeVersion . safeToJSON)
-- 'Value' of the provided type.
parseRemoveVersion :: forall a. SafeJSON a => Text -> a -> TestTree
parseRemoveVersion t val = parseAnd t go id
  where go = assertEqual "Unexpected result" $ (removeVersion . safeToJSON) val

parseAnd_ :: Text -> (Value -> Value) -> TestTree
parseAnd_ t = parseAnd t (const $ return () :: Value -> IO ())

parseAnd :: SafeJSON a => Text -> (a -> IO ()) -> (Value -> Value) -> TestTree
parseAnd t f g = testCase (T.unpack t) $
    A.decodeFileStrict "test/json/version.json"
      >>= maybe (assertFailure "couldn't read file")
                (either fail f . parseEither go)
  where go = A.withObject "test" $ \o -> do
                o .: t >>= safeFromJSON . g

