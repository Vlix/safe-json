{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module      : Data.Aeson.Safe
Copyright   : (c) 2019 Felix Paulusma
License     : MIT
Maintainer  : felix.paulusma@gmail.com
Stability   : experimental

This module contains homonyms of the "Data.Aeson" library's
encoding and decoding functions that, instead, use
"Data.SafeJSON"'s conversions.
This way, switching from "Data.Aeson" to "Data.SafeJSON" is
very easy. After any "Data.Aeson" imports, just add @.Safe@.

It also exports "Data.Aeson" itself for convenience, but still
hides 'parseJSON' and 'toJSON' so you will get errors if you
use them anywhere. That way you can explicitly decide where
to switch to 'safeFromJSON' or 'safeToJSON', or keep the
current "Data.Aeson" functions.
-}
module Data.Aeson.Safe (
    module Data.SafeJSON
  , module Aeson
  , decode
  , decode'
  , eitherDecode
  , eitherDecode'
  , encode
  , encodeFile

  , decodeStrict
  , decodeStrict'
  , eitherDecodeStrict
  , eitherDecodeStrict'
  , decodeFileStrict
  , decodeFileStrict'
  , eitherDecodeFileStrict
  , eitherDecodeFileStrict'
  , encodeStrict

  , parseEither
  , parseMaybe
  ) where


import Data.Aeson as Aeson hiding (
    decode
  , decode'
  , decodeFileStrict
  , decodeFileStrict'
  , decodeStrict
  , decodeStrict'
  , eitherDecode
  , eitherDecode'
  , eitherDecodeFileStrict
  , eitherDecodeFileStrict'
  , eitherDecodeStrict
  , eitherDecodeStrict'
  , encode
  , encodeFile

  , parseJSON
  , toJSON
  )
import qualified Data.Aeson as A (
    decode
  , decode'
  , decodeFileStrict
  , decodeFileStrict'
  , decodeStrict
  , decodeStrict'
  , eitherDecode
  , eitherDecode'
  , eitherDecodeFileStrict
  , eitherDecodeFileStrict'
  , eitherDecodeStrict
  , eitherDecodeStrict'
  , encode
  , encodeFile
  )
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.SafeJSON


-- These definitions might not be the most efficient way to
-- encode/decode JSON values (especially not as efficient as
-- Aeson itself), but that can be addressed later if needed.
--
-- Aeson does the majority of the work encoding to and decoding
-- from 'ByteString's anyway.

-- * Decoding and encoding of SafeJSON types

-- ** Lazy ByteString variants

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
decode :: SafeJSON a => LBS.ByteString -> Maybe a
decode lbs = A.decode lbs >>= parseMaybe safeFromJSON

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
decode' :: SafeJSON a => LBS.ByteString -> Maybe a
decode' lbs = A.decode' lbs >>= parseMaybe safeFromJSON

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecode :: SafeJSON a => LBS.ByteString -> Either String a
eitherDecode lbs = A.eitherDecode lbs >>= parseEither safeFromJSON

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecode' :: SafeJSON a => LBS.ByteString -> Either String a
eitherDecode' lbs = A.eitherDecode' lbs >>= parseEither safeFromJSON

-- | Encode a 'SafeJSON' value to a 'LBS.ByteString'.
encode :: SafeJSON a => a -> LBS.ByteString
encode = A.encode . safeToJSON

------------------------------------------------------
-- Strict variants
------------------------------------------------------

-- ** Strict ByteString variants

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
decodeStrict :: SafeJSON a => BS.ByteString -> Maybe a
decodeStrict lbs = A.decodeStrict lbs >>= parseMaybe safeFromJSON

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
decodeStrict' :: SafeJSON a => BS.ByteString -> Maybe a
decodeStrict' lbs = A.decodeStrict' lbs >>= parseMaybe safeFromJSON

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeStrict :: SafeJSON a => BS.ByteString -> Either String a
eitherDecodeStrict lbs = A.eitherDecodeStrict lbs >>= parseEither safeFromJSON

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeStrict' :: SafeJSON a => BS.ByteString -> Either String a
eitherDecodeStrict' lbs = A.eitherDecodeStrict' lbs >>= parseEither safeFromJSON

-- | Same as 'encode', but also calls 'LBS.toStrict', for convenience.
encodeStrict :: SafeJSON a => a -> BS.ByteString
encodeStrict = LBS.toStrict . encode

-- * Encoding to and decoding from files

-- | Try to decode a file to a 'SafeJSON' value.
decodeFileStrict :: SafeJSON a => FilePath -> IO (Maybe a)
decodeFileStrict fp = do
    mVal <- A.decodeFileStrict fp
    return $ mVal >>= parseMaybe safeFromJSON

-- | Try to decode a file to a 'SafeJSON' value.
decodeFileStrict' :: SafeJSON a => FilePath -> IO (Maybe a)
decodeFileStrict' fp = do
    mVal <- A.decodeFileStrict' fp
    return $ mVal >>= parseMaybe safeFromJSON

-- | Try to decode a file to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeFileStrict :: SafeJSON a => FilePath -> IO (Either String a)
eitherDecodeFileStrict fp = do
    eVal <- A.eitherDecodeFileStrict fp
    return $ eVal >>= parseEither safeFromJSON

-- | Try to decode a file to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeFileStrict' :: SafeJSON a => FilePath -> IO (Either String a)
eitherDecodeFileStrict' fp = do
    eVal <- A.eitherDecodeFileStrict' fp
    return $ eVal >>= parseEither safeFromJSON

-- | Encode a 'SafeJSON' value to a file.
encodeFile :: SafeJSON a => FilePath -> a -> IO ()
encodeFile fp = A.encodeFile fp . safeToJSON
