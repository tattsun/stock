{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.Static as S
import           System.Environment
import           Web.Scotty

--
import           Stock.Config

run :: Config -> IO ()
run config = scotty (configServerPort config) $ do
  middleware simpleCors
  middleware $ S.staticPolicy $ S.addBase (configStaticPath config) S.>-> (S.contains "/js/" S.<|> S.contains "/css/")
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
          run config
          return ()

test = loadConfig "./config.json" >>= run
