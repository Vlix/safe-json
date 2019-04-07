module Main where


import Test.Tasty as Tasty

import ConsistencyTests
import MigrationTests
import PrimitiveTests


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "\nSafeJSON"
    [ primitiveTests
    , migrationTests
    , consistencyTests
    ]
