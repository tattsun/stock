{-# LANGUAGE OverloadedStrings #-}
module Stock.User where

import           Control.Monad.IO.Class
import           Database.MongoDB
import           Stock.Config
import           Stock.Hash
import           Stock.Types

addUser :: (MonadIO m, Monad m) => String -> String -> String -> Action m (Maybe User)
addUser userid password name = do
  userbyid <- findUser userid
  userbyname <- findUserByName name
  maybe (return Nothing) (\_ -> go) userbyid
  where
    go = do
      ts <- liftIO $ getTimestamp
      saveUser $ user ts
      u <- findUser userid
      return $ u
    user ts = defaultUser { userId = userid
                          , userPassword = toHashBase64 password
                          , userName = name
                          , userTimestamp = ts
                          }

saveUser :: User -> Action m User
saveUser user = undefined

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
