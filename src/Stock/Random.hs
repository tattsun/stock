{-# LANGUAGE OverloadedStrings #-}
module Stock.Random
       ( randomStr
       ) where

import           Control.Monad
import           System.Random

strPool :: String
strPool = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

lenPool :: Int
lenPool = length strPool - 1

randomStr :: Int -> IO String
randomStr n = do
    lis <- replicateM n $ (getStdRandom $ randomR (0, lenPool) :: IO Int)
    return $ map (\ x -> strPool !! x) lis
