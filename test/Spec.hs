module Main where


import Data.Proxy
import Test.Tasty as Tasty

import Data.SafeJSON (Version)

import ConsistencyTests
import MigrationTests
import PrimitiveTests
import SafeAeson
import Version
import VersionNum


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "\nSafeJSON"
    [ numTest (Proxy :: Proxy (Version a))
    , versionFuncTests
    , primitiveTests
    , consistencyTests
    , encodeDecodeTests
    , migrationTests
    ]
