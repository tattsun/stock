{-# LANGUAGE OverloadedStrings #-}
module Stock.User where

import           Control.Monad.IO.Class
import           Database.MongoDB
import           Stock.Config
import           Stock.Hash
import           Stock.MongoDB
import           Stock.Types

userCollection = "users"

addUser :: (MonadIO m, Monad m) => Config -> String -> String -> String -> Action m (Maybe User)
addUser conf userid password name = do
  userbyid <- findUser userid
  userbyname <- findUserByName name
  maybe (return Nothing) (\_ -> go) userbyid
  where
    go = do
      user <- makeuser
      saveUser user
      u <- findUser userid
      return u
    makeuser = do
      ts <- liftIO getTimestamp
      return $ defaultUser { userId = userid
                           , userPassword = stHash conf password
                           , userName = name
                           , userTimestamp = ts
                           }

saveUser :: User -> Action m User
saveUser user = do
  save userCollection userBson
  u <- findUser $ userId user
  return u
  where userBson = obj2bson user

findUser :: String -> Action m (Maybe User)
findUser userid = undefined

findUserByName :: String -> Action m (Maybe User)
findUserByName name = undefined

updateUserProfile :: String -> String -> String -> Action m User
updateUserProfile userid name profile = undefined

changePassword :: String -> String -> Action m User
changePassword userid newpassword = undefined

authorizeUser :: String -> String -> Action m Bool
authorizeUser userid password = undefined
