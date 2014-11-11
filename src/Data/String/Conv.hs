{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.String.Conv
       ( SerializeString (..)
       ) where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU8
import qualified Data.ByteString.UTF8      as BU8
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL

-- *** Type Class

class SerializeString a where
  toString :: a -> String
  fromString :: String -> a

-- *** Instances

instance SerializeString [Char] where
  toString = id
  fromString = id

instance SerializeString B.ByteString where
  toString = BU8.toString
  fromString = BU8.fromString

instance SerializeString BL.ByteString where
  toString = BLU8.toString
  fromString = BLU8.fromString

instance SerializeString T.Text where
  toString = T.unpack
  fromString = T.pack

instance SerializeString TL.Text where
  toString = TL.unpack
  fromString = TL.pack
