{-# LANGUAGE OverloadedStrings #-}
module Stock.Types.DateTime
       ( UnixTime(..)
       , getTimestamp
       , timeToString
       , parseToTime
       ) where

import           Control.Applicative
import           Data.String.Conv
import           Data.UnixTime

fmt = "%Y-%m-%d %H:%M:%S"

getTimestamp :: IO String
getTimestamp = getUnixTime >>= timeToString

timeToString :: UnixTime -> IO String
timeToString ut = toString <$> formatUnixTime fmt ut

parseToTime :: String -> UnixTime
parseToTime str = parseUnixTime fmt (fromString str)
