{-# LANGUAGE OverloadedStrings #-}
module Stock.Hash
       ( toHashBase64
       ) where

import           Codec.Digest.SHA
import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import           Data.String.Conv

toHash :: ByteString -> ByteString
toHash = hash SHA512

toHashBase64 :: String -> String
toHashBase64 = toString . Base64.encode . toHash . (fromString :: String -> ByteString)

t = toHashBase64 "test"
