{-# LANGUAGE OverloadedStrings #-}
module Stock.Types.DateTime where

import           Control.Applicative
import           Data.String.Conv
import           Data.UnixTime

data DateTime = DateTime { dtYear  :: Int
                         , dtMonth :: Int
                         , dtHour  :: Int
                         , dtMin   :: Int
                         , dtSec   :: Int
                         } deriving (Show, Eq)

getTimestamp :: IO String
getTimestamp = toString <$> (getUnixTime >>= formatUnixTime "%Y-%m-%d %H:%M:%S")
