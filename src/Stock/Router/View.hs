{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stock.Router.View where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Text
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (hamletFile)
import           Web.Scotty

--
import           Stock.Article
import           Stock.Config
import           Stock.MongoDB
import           Stock.Types

viewIndex conf = do
  get "/" $ do
    articles <- liftIO . runMongo conf $ findArticles conf Nothing Nothing Public Nothing Nothing
    html $ renderHtml $ $(hamletFile "views/index.hamlet") undefined
