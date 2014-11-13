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
import           Stock.Tags
import           Stock.Types

getHead conf title = $(hamletFile $ hamletPath "head")
getHeader conf = $(hamletFile $ hamletPath "header")
getFooter conf = $(hamletFile $ hamletPath "footer")

getArticleView conf article = $(hamletFile $ hamletPath "article.comp")

cutTitle len str = take len str ++ if length str > len
                                   then "..."
                                   else []

viewIndex conf = do
  tags <- liftIO . runMongo conf $ getTagCounts
  recents <- liftIO . runMongo conf $ findArticles conf Nothing (Just 10) Public Nothing Nothing Nothing
  let rightmenu = $(hamletFile $ hamletPath "menu.comp")
  get "/" $ do
    cursorM <- paramMaybe "cursor"
    articles <- liftIO . runMongo conf $ findArticles conf cursorM Nothing Public Nothing Nothing Nothing
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/archive/:year/:month" $ do
    cursorM <- paramMaybe "cursor"
    year <- param "year"
    month <- param "month"
    endM <- liftIO $ getLastDayOfMonth year month
    let start = getFirstDayOfMonth year month
        period = maybe Nothing (\end -> Just (start, end)) endM
    articles <- liftIO . runMongo conf $ findArticles conf cursorM Nothing Public period Nothing Nothing
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/users/:user" $ do
    cursorM <- paramMaybe "cursor"
    user <- param "user"
    articles <- liftIO . runMongo conf $ findArticles conf cursorM Nothing Public Nothing Nothing (Just user)
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/tag/:tag" $ do
    cursorM <- paramMaybe "cursor"
    tag <- param "tag"
    articles <- liftIO . runMongo conf $ findArticles conf cursorM Nothing Public Nothing (Just [tag]) Nothing
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/:id" $ do
    id <- param "id"
    articleM <- liftIO . runMongo conf $ findArticle id
    maybe (html "404 Error") (\article ->
                               let articles = [article]
                               in html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
                             ) articleM
