{-# LANGUAGE OverloadedStrings #-}
module Stock.Router.API where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.List.Split
import           Data.Maybe
import           Web.Scotty

--
import           Stock.Article
import           Stock.Config
import           Stock.MongoDB
import           Stock.Scotty
import           Stock.Types
import           Stock.User

-- debug
import           Debug.Trace

japiUser conf = do
  post "/user" $ do
    userid <- param "userid"
    password <- param "password"
    name <- param "name"
    res <- liftIO . runMongo conf $ addUser conf userid password name
    maybe (json $ toStatus DataDuplicated "") (\u -> do
                                                  token <- liftIO . runMongo conf $ generateToken conf userid
                                                  json $ toStatus Token token ) res
  post "/user/login" $ do
    userid <- param "userid"
    password <- param "password"
    res <- liftIO . runMongo conf $ authorizeUser conf userid password
    if res
      then do token <- liftIO . runMongo conf $ generateToken conf userid
              json $ toStatus Token token
      else json $ toStatus Unauthorized ""
  post "/user/token" $ do
    userid <- param "userid"
    token <- param "token"
    res <- liftIO . runMongo conf $ authorizeToken userid token
    if res
      then json $ toStatus Success ""
      else json $ toStatus Unauthorized ""

japiArticle conf = do
  post "/article" $ do
    userid <- param "userid"
    token <- param "token"
    region <- read <$> param "region"
    title <- htmlEscape <$> param "title"
    tags <- splitOn "," <$> htmlEscape <$> param "tags"
    body <- htmlEscape <$> param "body"
    auth <- liftIO . runMongo conf $ authorizeToken userid token
    if auth
      then do user <- liftIO . runMongo conf $ (fromJust <$> findUser userid)
              a <- liftIO $ runMongo conf $ postArticle region title userid (userName user) tags body
              json $ toStatus Success (articleId a)
      else json $ toStatus Unauthorized ""
  post "/article/:articleid" $ do
    articleid <- param "articleid"
    userid <- param "userid"
    token <- param "token"
    region <- read <$> param "region"
    title <- htmlEscape <$> param "title"
    tags <- splitOn "," <$> htmlEscape <$> param "tags"
    body <- htmlEscape <$> param "body"
    auth <- liftIO . runMongo conf $ authorizeToken userid token
    if auth
      then do user <- liftIO . runMongo conf $ (fromJust <$> findUser userid)
              a <- liftIO $ runMongo conf $ updateArticle region articleid title tags body
              json $ maybe (toStatus Failed "") (\ar -> toStatus Success (articleId ar)) a
      else json $ toStatus Unauthorized ""
