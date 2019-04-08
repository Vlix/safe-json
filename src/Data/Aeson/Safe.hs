module Data.Aeson.Safe where


import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.SafeJSON (SafeJSON, safeFromJSON, safeToJSON)


-- These definitions might not be the most efficient way to
-- encode/decode JSON values (especially not as efficient as
-- Aeson itself), but that can be addressed later if needed.
--
-- Aeson does the majority of the work encoding to and decoding
-- from 'ByteString's anyway.

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
decode :: SafeJSON a => LBS.ByteString -> Maybe a
decode lbs = A.decode lbs >>= A.parseMaybe safeFromJSON

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
decode' :: SafeJSON a => LBS.ByteString -> Maybe a
decode' lbs = A.decode' lbs >>= A.parseMaybe safeFromJSON

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecode :: SafeJSON a => LBS.ByteString -> Either String a
eitherDecode lbs = A.eitherDecode lbs >>= A.parseEither safeFromJSON

-- | Try to decode a 'LBS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecode' :: SafeJSON a => LBS.ByteString -> Either String a
eitherDecode' lbs = A.eitherDecode' lbs >>= A.parseEither safeFromJSON

-- | Encode a 'SafeJSON' value to a 'LBS.ByteString'.
encode :: SafeJSON a => a -> LBS.ByteString
encode = A.encode . safeToJSON

-- | Encode a 'SafeJSON' value to a file.
encodeFile :: SafeJSON a => FilePath -> a -> IO ()
encodeFile fp = A.encodeFile fp . safeToJSON


------------------------------------------------------
-- Strict variants
------------------------------------------------------

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
decodeStrict :: SafeJSON a => BS.ByteString -> Maybe a
decodeStrict lbs = A.decodeStrict lbs >>= A.parseMaybe safeFromJSON

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
decodeStrict' :: SafeJSON a => BS.ByteString -> Maybe a
decodeStrict' lbs = A.decodeStrict' lbs >>= A.parseMaybe safeFromJSON

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeStrict :: SafeJSON a => BS.ByteString -> Either String a
eitherDecodeStrict lbs = A.eitherDecodeStrict lbs >>= A.parseEither safeFromJSON

-- | Try to decode a 'BS.ByteString' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeStrict' :: SafeJSON a => BS.ByteString -> Either String a
eitherDecodeStrict' lbs = A.eitherDecodeStrict' lbs >>= A.parseEither safeFromJSON

-- | Try to decode a 'FilePath' to a 'SafeJSON' value.
decodeFileStrict :: SafeJSON a => FilePath -> IO (Maybe a)
decodeFileStrict fp = do
    mVal <- A.decodeFileStrict fp
    return $ mVal >>= A.parseMaybe safeFromJSON

-- | Try to decode a 'FilePath' to a 'SafeJSON' value.
decodeFileStrict' :: SafeJSON a => FilePath -> IO (Maybe a)
decodeFileStrict' fp = do
    mVal <- A.decodeFileStrict' fp
    return $ mVal >>= A.parseMaybe safeFromJSON

-- | Try to decode a 'FilePath' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeFileStrict :: SafeJSON a => FilePath -> IO (Either String a)
eitherDecodeFileStrict fp = do
    eVal <- A.eitherDecodeFileStrict fp
    return $ eVal >>= A.parseEither safeFromJSON

-- | Try to decode a 'FilePath' to a 'SafeJSON' value.
--   Produces an error message on failure.
eitherDecodeFileStrict' :: SafeJSON a => FilePath -> IO (Either String a)
eitherDecodeFileStrict' fp = do
    eVal <- A.eitherDecodeFileStrict' fp
    return $ eVal >>= A.parseEither safeFromJSON

-- | Same as 'encode', but also calls 'LBS.toStrict', for convenience.
encodeStrict :: SafeJSON a => a -> BS.ByteString
encodeStrict = LBS.toStrict . encode
