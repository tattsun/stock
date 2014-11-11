{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.Static as S
import           System.Environment
import           Web.Scotty

--
import           Stock.Config
import           Stock.Router
import           Stock.Scotty

-- debug
import           Codec.Binary.UTF8.String
import           Data.String.Conv
import qualified Data.Text.Lazy                as T
import           Stock.MongoDB
import           Stock.Types

middlewares conf = do
  middleware simpleCors
  middleware $ S.staticPolicy $ S.addBase (configStaticPath conf) S.>-> (S.contains "/js/" S.<|> S.contains "/css/"
                                                                        S.<|> S.contains "/img/")

run :: Config -> IO ()
run conf = scotty (configServerPort conf) $ do
  middlewares conf
  japiUser conf
  japiArticle conf
  viewIndex conf

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
