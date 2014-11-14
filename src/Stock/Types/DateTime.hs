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
       , getFirstTimeOfMonthUT
       , getLastTimeOfMonthUT
       , getRecentMonths
       , timeToYM
       , timeToY
       , timeToM
       ) where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Data.String.Conv
import           Data.UnixTime
import           Text.Printf
import           Text.Regex

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


getFirstTimeOfMonthUT :: UnixTime -> IO UnixTime
getFirstTimeOfMonthUT ut = do
  timeString <- timeToString ut
  let (ymd:hms:_) = words timeString
      (ys:ms:_) = splitRegex (mkRegex "-") ymd
  return $ parseToTime $ ys ++ "-" ++ ms ++ "-01 00:00:00"

getLastTimeOfMonthUT :: UnixTime -> IO UnixTime
getLastTimeOfMonthUT ut = do
  timeString <- timeToString ut
  let (ymd:hms:_) = words timeString
      (ys:ms:_) = splitRegex (mkRegex "-") ymd
      (y, m) = (read ys, read ms) :: (Int, Int)
      nextMonthStr = if m == 12
                     then (show $ y+1) ++ "-01-01 00:00:00"
                     else ys ++ "-" ++ (show $ m + 1) ++ "-01 00:00:00"
      nextMonth = parseToTime nextMonthStr
      lastDay = addUnixDiffTime nextMonth (secondsToUnixDiffTime $ -1)
  return lastDay

getRecentMonths :: Int -> IO [UnixTime]
getRecentMonths monthNum = do
  fstDay <- getFirstTimeOfMonthUT =<< getUnixTime
  months <- getUnixTime >>= getLoop 2 [fstDay]
  return $ reverse months
  where
    getLoop i uts ut
      | i > monthNum = return uts
      | otherwise = do
          lastMonth <- getLastMonth ut
          getLoop (i+1) (lastMonth:uts) lastMonth

getLastMonth :: UnixTime -> IO UnixTime
getLastMonth ut = do
  timeString <- timeToString ut
  let (ymd:hms:_) = words timeString
      (ys:ms:_) = splitRegex (mkRegex "-") ymd
      (y, m) = (read ys, read ms) :: (Int, Int)
      (lastY, lastM) = if m == 1
                       then (y - 1, 12)
                       else (y, m - 1)
  return $ parseToTime $ show lastY ++ "-" ++ show lastM ++ "-01 00:00:00"

timeToYM :: UnixTime -> IO String
timeToYM ut = toString <$> formatUnixTime "%Y-%m" ut

timeToY :: UnixTime -> IO String
timeToY ut = toString <$> formatUnixTime "%Y" ut

timeToM :: UnixTime -> IO String
timeToM ut = toString <$> formatUnixTime "%m" ut

--t s = timeToString =<< (getLastMonth . test $ s)
--test = parseToTime . (++"-01 00:00:00")

--t = getFirstDayOfMonth 2014 5
t2 = sequence $ map (getLastDayOfMonth 2014) [1..15]
