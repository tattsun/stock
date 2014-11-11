{-# LANGUAGE OverloadedStrings #-}
module Stock.Router.API where

import           Control.Monad.IO.Class
import           Web.Scotty

--
import           Stock.Article
import           Stock.Config
import           Stock.MongoDB
import           Stock.Scotty
import           Stock.User

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
