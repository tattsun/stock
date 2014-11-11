{-# LANGUAGE OverloadedStrings #-}
module Stock.Types.DateTime
       ( UnixTime(..)
       , getUnixTime
       , getTimestamp
       , getTimestampShiftSec
       , timeToString
       , parseToTime
       ) where

import           Control.Applicative
import           Data.String.Conv
import           Data.UnixTime

fmt = "%Y-%m-%d %H:%M:%S"

getTimestamp :: IO String
getTimestamp = getUnixTime >>= timeToString

getTimestampShiftSec :: Integer -> IO String
getTimestampShiftSec sec = flip addUnixDiffTime (secondsToUnixDiffTime sec) <$> getUnixTime
                           >>= timeToString

timeToString :: UnixTime -> IO String
timeToString ut = toString <$> formatUnixTime fmt ut

parseToTime :: String -> UnixTime
parseToTime str = parseUnixTime fmt (fromString str)
