{-# LANGUAGE OverloadedStrings #-}
module Stock.MongoDB
       ( runMongo
       , obj2bson
       , bson2obj
       ) where

import           Control.Monad.IO.Class
import qualified Data.Aeson             as A
import           Data.AesonBson
import qualified Data.Bson              as B
import           Database.MongoDB

--
import           Data.String.Conv
import           Stock.Config
import           Stock.Types

runMongo :: Config -> Action IO a -> IO a
runMongo conf act = do
  pipe <- connect (host $ configMongoDBHostName conf)
  res <- access pipe master (fromString $ configMongoDBName conf) act
  close pipe
  return res

obj2bson :: (A.ToJSON a) => a -> B.Document
obj2bson = getDoc . bsonifyValue . A.toJSON
  where getDoc (Doc d) = d

bson2obj :: (A.FromJSON a) => B.Document -> Maybe a
bson2obj bson = let res = A.fromJSON . aesonifyValue . Doc $ bson
                in case res of
                    A.Error _ -> Nothing
                    A.Success o -> Just o
