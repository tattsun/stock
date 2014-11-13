{-# LANGUAGE OverloadedStrings #-}
module Stock.Types.DateTime
       ( UnixTime(..)
       , getUnixTime
       , getTimestamp
       , getTimestampShiftSec
       , timeToString
       , parseToTime
       , getFirstDayOfMonth
       , getLastDayOfMonth
       ) where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Data.String.Conv
import           Data.UnixTime
import           Text.Printf

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

getFirstDayOfMonth :: Int -> Int -> UnixTime
getFirstDayOfMonth year month = parseToTime (concat $ [show year, "-", show month, "-01 00:00:00"])


isMonth :: Int -> Int -> UnixTime -> IO Bool
isMonth year month ut = do
  ym <- toString <$> formatUnixTime "%Y-%m" ut
  let result = (concat [show year, "-", printf "%02d" month]) == ym
  return result

getLastDayOfMonth :: Int -> Int -> IO (Maybe UnixTime)
getLastDayOfMonth year month = lastDay
  where lastDays = [28,29,30,31]
        lastDayUts = sort $ map (\day -> parseToTime
                                         $ concat [show year, "-", show month, "-", show day, " 23:59:59"]) lastDays
        lastDay = listToMaybe <$> filterIO (isMonth year month) lastDayUts

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO f xs = do
  bools <- sequence $ map f xs
  let withBool = zip bools xs
  return $ map (\(_,x) -> x) $ filter (\(b, _) -> b) withBool

--t = getFirstDayOfMonth 2014 5
t2 = sequence $ map (getLastDayOfMonth 2014) [1..15]
