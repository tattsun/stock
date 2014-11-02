{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Network.Wai.Middleware.Cors
import           Stock.Config
import           System.Environment
import           Web.Scotty

run :: Config -> IO ()
run config = scotty (configServerPort config) $ do
  middleware simpleCors
  get "/" $ do
    html $ "Hello, World"

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then putStrLn "stock-server <configpath>"
    else go (args !! 0)
  where go configPath = do
          config <- loadConfig configPath
          putStrLn $ show config
          return ()

test = loadConfig "./config.json" >>= run
