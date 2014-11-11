{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Stock.Config
       ( Config (..)
       , loadConfig
       ) where

import           Control.Applicative
import           Control.Exception
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.Maybe
import           Data.String.Conv
import           Data.Typeable
import           Stock.Types

data ConfigException = ConfigNotFoundException FilePath | ConfigInvalidException FilePath deriving Typeable
instance Show ConfigException where
  show (ConfigNotFoundException p) = concat ["configuration file, ", p ," is not found."]
  show (ConfigInvalidException p) = concat ["configuration file, ", p ," is invalid."]
instance Exception ConfigException

loadConfig :: FilePath -> IO Config
loadConfig p = do
  file <- readFile p `catch` (\(SomeException e) -> throw $ ConfigNotFoundException p)
  let configM = decode . fromString $ file
  maybe (throw $ ConfigInvalidException p) (return . fromJust) configM

test = loadConfig "./test.json"
test2 = loadConfig "./config.json"
