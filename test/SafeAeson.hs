{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SafeAeson where


import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import System.IO.Temp (withTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson.Safe
import Data.SafeJSON
import Types


encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "Encoding/Decoding (Data.Aeson.Safe)"
    [ parseFile   @NoVersion "decodeFile  (noversion)" noVersionPath
    , parseFile'  @NoVersion "decodeFile' (noversion)" noVersionPath
    , parseBigFile           "decodeFile (primitives)" primitivesPath
    , writeToFile            "encodeFile"
    , parseValue  @NoVersion "decode      (noversion)"
    , parseValue' @NoVersion "decode'     (noversion)"
    , encodeType             "encode"
    ]


jsonPath :: FilePath
jsonPath = "test/json"

noVersionPath :: FilePath
noVersionPath = jsonPath ++ "/noversion.json"

primitivesPath :: FilePath
primitivesPath = jsonPath ++ "/primitives.json"

noVersionType :: NoVersion
noVersionType = NoVersion 23986

parseBigFile :: String -> FilePath -> TestTree
parseBigFile s fp = testCase s $ do
    a <- decodeFileStrict @Value fp
    b <- A.decodeFileStrict fp
    b @=? a
    c <- decodeFileStrict' @Value fp
    d <- A.decodeFileStrict' fp
    d @=? c
    e <- eitherDecodeFileStrict @Value fp
    x <- A.eitherDecodeFileStrict fp
    x @=? e
    y <- eitherDecodeFileStrict' @Value fp
    z <- A.eitherDecodeFileStrict' fp
    z @=? y

parseValue :: forall a. (SafeJSON a, Eq a, Show a) => String -> TestTree
parseValue s = testCase s $ do
    lbs <- LBS.readFile noVersionPath
    decode @a       lbs @=? A.decode       lbs
    eitherDecode @a lbs @=? A.eitherDecode lbs

    bs <- BS.readFile noVersionPath
    decodeStrict @a       bs @=? A.decodeStrict       bs
    eitherDecodeStrict @a bs @=? A.eitherDecodeStrict bs

parseValue' :: forall a. (SafeJSON a, Eq a, Show a) => String -> TestTree
parseValue' s = testCase s $ do
    lbs <- LBS.readFile noVersionPath
    decode' @a       lbs @=? A.decode'       lbs
    eitherDecode' @a lbs @=? A.eitherDecode' lbs

    bs <- BS.readFile noVersionPath
    decodeStrict' @a       bs @=? A.decodeStrict'       bs
    eitherDecodeStrict' @a bs @=? A.eitherDecodeStrict' bs

parseFile :: forall a. (SafeJSON a, Eq a, Show a) => String -> FilePath -> TestTree
parseFile s fp = testCase s $ do
    a <- decodeFileStrict @Value fp
    b <- A.decodeFileStrict fp
    b @=? a

    c <- decodeFileStrict @a fp
    d <- A.decodeFileStrict fp
    d @=? c

    e <- eitherDecodeFileStrict @Value fp
    x <- A.eitherDecodeFileStrict fp
    x @=? e

    y <- eitherDecodeFileStrict @a fp
    z <- A.eitherDecodeFileStrict fp
    z @=? y

parseFile' :: forall a. (SafeJSON a, Eq a, Show a) => String -> FilePath -> TestTree
parseFile' s fp = testCase s $ do
    a <- decodeFileStrict' @Value fp
    b <- A.decodeFileStrict' fp
    b @=? a

    c <- decodeFileStrict' @a fp
    d <- A.decodeFileStrict' fp
    d @=? c

    e <- eitherDecodeFileStrict' @Value fp
    x <- A.eitherDecodeFileStrict' fp
    x @=? e

    y <- eitherDecodeFileStrict' @a fp
    z <- A.eitherDecodeFileStrict' fp
    z @=? y

writeToFile :: String -> TestTree
writeToFile s = testCase s $ do
    withTempDirectory "test/json" "encode" $ \dir -> do
        let file1 = dir ++ "/test.json"
            file2 = dir ++ "/test2.json"
        encodeFile file1 noVersionType
        A.encodeFile file2 noVersionType
        x <- A.decodeFileStrict @NoVersion file1
        y <- A.decodeFileStrict file2
        y @=? x

encodeType :: String -> TestTree
encodeType s = testCase s $
    encode noVersionType @=? A.encode noVersionType
