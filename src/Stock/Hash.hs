{-# LANGUAGE OverloadedStrings #-}
module Stock.Hash
       ( stHash
       ) where

import           Codec.Digest.SHA
import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import           Data.String.Conv
import           Stock.Types

toHash :: ByteString -> ByteString
toHash = hash SHA512

toHashBase64 :: String -> String
toHashBase64 = toString . Base64.encode . toHash . (fromString :: String -> ByteString)

stHash :: Config -> String -> String
stHash conf pass = toHashBase64 (configPasswordSalt conf ++ pass)

t = toHashBase64 "test"
