{-# LANGUAGE OverloadedStrings #-}
module Stock.User
       ( addUser
       , findUser
       , updateUserProfile
       , authorizeUser
       , generateToken
       , authorizeToken
       ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Database.MongoDB       hiding (addUser)

--
import           Stock.Config
import           Stock.Hash
import           Stock.MongoDB
import           Stock.Random
import           Stock.Types

----------------------------------------------------------------------
-- *** Database

userCollection = "users"

addUser :: (MonadIO m, Monad m, Functor m, Applicative m) =>
           Config -> String -> String -> String -> Action m (Maybe User)
addUser conf userid password name = do
  isduplicate <- ((||) <$> isExistsUserId userid) <*> isExistsUserName name
  if isduplicate
    then return Nothing
    else makeuser >>= saveUser
    where
      makeuser = do
        ts <- liftIO getTimestamp
        return $ defaultUser { userId = userid
                             , userPassword = stHash conf password
                             , userName = name
                             , userTimestamp = ts
                             }

saveUser :: (MonadIO m, Monad m, Functor m) => User -> Action m (Maybe User)
saveUser user = do
  isexists <- isExistsUserId (userId user)
  if isexists
    then doReplace >> findUser (userId user)
    else doInsert >> findUser (userId user)
  where
    doReplace = replace (select ["id" =: userId user] userCollection) userBson
    doInsert = insert userCollection userBson
    userBson = obj2bson user

isExistsUserId :: (MonadIO m, Functor m) => String -> Action m Bool
isExistsUserId userId = maybe False (const True) <$> findUser userId

isExistsUserName :: (MonadIO m, Functor m) => String -> Action m Bool
isExistsUserName name = maybe False (const True) <$> findUserByName name

findUser :: (MonadIO m, Functor m) => String -> Action m (Maybe User)
findUser userid = maybe Nothing bson2obj <$> findOne (select ["id" =: userid] userCollection)

findUserByName :: (MonadIO m, Functor m) => String -> Action m (Maybe User)
findUserByName name = maybe Nothing bson2obj <$> findOne (select ["name" =: name] userCollection)

updateUserProfile :: (Monad m, Functor m, MonadIO m) =>
                     String -> String -> String -> Action m (Maybe User)
updateUserProfile userid name profile = do
  user <- maybe Nothing (\u -> Just u { userName = name, userProfile = profile }) <$> findUser userid
  maybe (return Nothing) saveUser user

--changePassword :: String -> String -> Action m User
--changePassword userid newpassword = undefined

authorizeUser :: (Monad m, Functor m, MonadIO m) =>
                 Config -> String -> String -> Action m Bool
authorizeUser conf userid password = do
  isauthorized <- maybe Nothing (\u -> Just $ userPassword u == stHash conf password) <$> findUser userid
  return $ maybe False id isauthorized

generateToken :: (MonadIO m, Functor m) => Config -> String -> Action m String
generateToken conf userid = do
  token <- liftIO $ randomStr 50
  tokenLimit <- liftIO . getTimestampShiftSec . fromIntegral $ configTokenLimitSec conf
  user <- maybe Nothing (\u -> Just $ u { userToken = token, userTokenLimit = tokenLimit }) <$> findUser userid
  maybe (return Nothing) saveUser user
  return token

authorizeToken :: (MonadIO m, Functor m) =>
                  String -> String -> Action m Bool
authorizeToken userid token = do
  user <- findUser userid
  maybe (return False) go user
  where
    go user = do
      let tokenR = userToken user
          limit = parseToTime $ userTokenLimit user
      now <- liftIO $ getUnixTime
      if now > limit
        then return False
        else if tokenR == token
             then return True
             else return False
