{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import Data.Proxy
import Test.Tasty as Tasty

import Data.SafeJSON (Version)
import Data.SafeJSON.Test()
import ConsistencyTests
import MigrationTests
import PrimitiveTests
import VersionNum


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "\nSafeJSON"
    [ {-primitiveTests
    ,-} consistencyTests
    -- , migrationTests
    -- , numTest (Proxy :: Proxy (Version a))
    ]


{-
Needed to be tested:

- Primitive types (noVersion) should behave the same as just Aeson
    in the case of noVersion Base types:
    - toJSON a @=? safeToJSON a
    - parseJSON a @=? safeFromJSON a
    container-like types should handle different versions:
    - SafeJSON a => [a] should be able to handle different versions of 'a'

- a @=? safeFromJSON (safeToJSON a)
    - regardless of position in the chain

- migrate a @=? safeFromJSON (safeToJSON a)
    - also with 'Reverse a'

- Data.Aeson.Safe functions should behave as expected

- Consistency should be reliable
    - show double version numbers/loops when inconsistent
    - succeed if the chain is valid
    - valid chain also needs any 'extended_*' instance's @MigrateFrom (Reverse a)@
      to be an 'extension' or 'extended_extension'

- Num class of Version should be consistent with Num laws

-}
