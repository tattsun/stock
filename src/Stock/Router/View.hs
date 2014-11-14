{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stock.Router.View
       ( views
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.List
import           Data.Maybe
import           Data.String.Conv
import qualified Data.Text.Lazy                as T
import           Text.Blaze
import           Text.Blaze.Html.Renderer.Text
import           Text.Hamlet
import           Web.Scotty
import           Web.Scotty.Cookie
import           Web.Scotty.Internal.Types     (ActionT (..), ScottyError (..),
                                                ScottyT (..))

--
import           Stock.Article
import           Stock.Config
import           Stock.MongoDB
import           Stock.Scotty
import           Stock.Tags
import           Stock.Types
import           Stock.User

----------------------------------------------------------------------
-- *** Generic view components

getHead conf title = $(hamletFile $ hamletPath "head")
getHeader conf = $(hamletFile $ hamletPath "header")
getFooter conf = $(hamletFile $ hamletPath "footer")

getArticlesView conf articles region = $(hamletFile $ hamletPath "articles.comp")
getArticleView conf article region = $(hamletFile $ hamletPath "article.comp")

data RecentMonth = RecentMonth { rmYear         :: String
                               , rmMonth        :: String
                               , rmYM           :: String
                               , rmArticleCount :: Int
                               } deriving (Show, Eq)
utToRecentMonth :: Config -> ShowRegion -> UnixTime -> IO RecentMonth
utToRecentMonth conf region ut = do
  ym <- timeToYM ut
  y <- timeToY ut
  m <- timeToM ut
  start <- getFirstTimeOfMonthUT ut
  end <- getLastTimeOfMonthUT ut
  cnt <- runMongo conf $ countArticles conf region defaultOptions { optionsPeriod = Just (start, end) }
  return $ RecentMonth y m ym cnt
getRightMenu conf region = do
  (tags, recents) <- liftIO $ tagAndRecent
  recentUTs <- liftIO $ getRecentMonths (configArchiveListNum conf)
  recentMonths <- liftIO $ sequence $ map (utToRecentMonth conf region) recentUTs
  return $(hamletFile $ hamletPath "menu.comp")
  where
    tagAndRecent = runMongo conf $ do
      tags <- getTagCounts region
      recents <- findArticles conf region defaultOptions {optionsLimit = Just 10}
      return (tags, recents)


----------------------------------------------------------------------

cutTitle len str = take len str ++ if length str > len
                                   then "..."
                                   else []

isAuthorized conf = do
  userid <- maybe Nothing (Just . toString) <$> getCookie "userid"
  token <- maybe Nothing (Just . toString) <$> getCookie "token"
  maybe (return False) (\id -> maybe (return False) (\tk -> go id tk) token) userid
  where go userid token = do
          isauthorized <- liftIO $ runMongo conf $ authorizeToken userid token
          return isauthorized

getShowRegion conf = do
  isauthorized <- isAuthorized conf
  if isauthorized
    then return Private
    else return Public

----------------------------------------------------------------------
-- *** View Controllers

views conf = do
  viewEtc conf
  viewUsers conf
  viewIndex conf

viewEtc conf = do
  get "/login" $ do
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    let leftcolumn = $(hamletFile $ hamletPath "login.comp")
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined

viewUsers conf = do
  get "/post" $ do
    let isUpdate = False
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    let article = defaultArticle
    let leftcolumn = if region /= Private
                     then let errorTitle = "ページが存在しません"
                              errorDetail = "ページが存在しません！"
                          in $(hamletFile $ hamletPath "error.comp")
                     else $(hamletFile $ hamletPath "post.comp")
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/edit/:articleid" $ do
    let isUpdate = True
    articleid <- param "articleid"
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    articleM <- liftIO . runMongo conf $ findArticle articleid
    let leftcolumn = if region /= Private || isNothing articleM
                     then let errorTitle = "ページが存在しません"
                              errorDetail = "ページが存在しません！"
                          in $(hamletFile $ hamletPath "error.comp")
                     else let article = fromJust articleM
                          in $(hamletFile $ hamletPath "post.comp")
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined

viewIndex conf = do
  get "/" $ do
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    cursorM <- paramMaybe "cursor"
    articles <- liftIO . runMongo conf $ findArticles conf region defaultOptions { optionsCursor = cursorM }
    let leftcolumn = getArticlesView conf articles region
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/archive/:year/:month" $ do
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    cursorM <- paramMaybe "cursor"
    year <- param "year"
    month <- param "month"
    endM <- liftIO $ getLastDayOfMonth year month
    let start = getFirstDayOfMonth year month
        period = maybe Nothing (\end -> Just (start, end)) endM
    articles <- liftIO . runMongo conf $ findArticles conf region defaultOptions { optionsCursor = cursorM
                                                                                 , optionsPeriod = period
                                                                                 }
    let leftcolumn = getArticlesView conf articles region
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/users/:user" $ do
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    cursorM <- paramMaybe "cursor"
    user <- param "user"
    articles <- liftIO . runMongo conf $ findArticles conf region defaultOptions { optionsCursor = cursorM
                                                                                 , optionsUser = Just user
                                                                                 }
    let leftcolumn = getArticlesView conf articles region
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/tag/:tag" $ do
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    cursorM <- paramMaybe "cursor"
    tag <- param "tag"
    articles <- liftIO . runMongo conf $ findArticles conf region defaultOptions { optionsCursor = cursorM
                                                                                 , optionsTags = Just [tag]
                                                                                 }
    let leftcolumn = getArticlesView conf articles region
    html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
  get "/:id" $ do
    region <- getShowRegion conf
    rightmenu <- getRightMenu conf region
    id <- param "id"
    articleM <- liftIO . runMongo conf $ findArticle id
    maybe (html "404 Error") (\article ->
                               let articles = [article]
                                   leftcolumn = getArticlesView conf articles region
                               in html $ renderHtml $ $(hamletFile $ hamletPath "index") undefined
                             ) articleM
