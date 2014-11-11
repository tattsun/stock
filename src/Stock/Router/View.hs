{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stock.Router.View where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text.Lazy                as T
import           Text.Blaze
import           Text.Blaze.Html.Renderer.Text
import           Text.Hamlet
import           Web.Scotty

--
import           Stock.Article
import           Stock.Config
import           Stock.MongoDB
import           Stock.Scotty
import           Stock.Types

getHead conf title = $(hamletFile $ hamletPath "head")
getHeader conf = $(hamletFile $ hamletPath "header")
getFooter conf = $(hamletFile $ hamletPath "footer")

viewIndex conf = do
  get "/" $ do
    articles <- liftIO . runMongo conf $ findArticles conf Nothing Nothing Public Nothing Nothing
    html $ renderHtml $ $(hamletFileReload $ hamletPath "index") undefined
