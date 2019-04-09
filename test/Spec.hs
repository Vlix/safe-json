{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import Data.Proxy
import Test.Tasty as Tasty

import Data.SafeJSON (Version)

import ConsistencyTests
import MigrationTests
import PrimitiveTests
import VersionNum


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "\nSafeJSON"
    [ numTest (Proxy :: Proxy (Version a))
    , primitiveTests
    , consistencyTests
    , migrationTests
    -- , encodeDecodeTests -- These should test the Data.Aeson.Safe functions
    ]


{-
Needed to be tested:

- V  Num class of Version should be consistent with Num laws

- V  Primitive types (noVersion) should behave the same as just Aeson
        in the case of noVersion Base types:
        V  - toJSON a @=? safeToJSON a
        V  - parseJSON a @=? safeFromJSON a

- V  a @=? safeFromJSON (safeToJSON a)
        V  - all primitives
        V  - other types regardless of position in the chain

- V  migrate a @=? safeFromJSON (safeToJSON a)
        - also with 'Reverse a'

- Data.Aeson.Safe functions should behave as expected

- Consistency should be reliable
    V  - show double version numbers/loops when inconsistent
    V  - succeed if the chain is valid
    V  - chain is also inconsistent if 2 types within (not the one being checked) share version numbers
    [] - container-like types should handle different versions:
         SafeJSON a => [a] should be able to handle different versions of 'a'


-}
