{-# LANGUAGE OverloadedStrings #-}
module Stock.MongoDB
       ( obj2bson
       , bson2obj
       ) where

import qualified Data.Aeson       as A
import           Data.AesonBson
import qualified Data.Bson        as B
import           Database.MongoDB

--
import           Stock.Types

obj2bson :: (A.ToJSON a) => a -> B.Document
obj2bson = getDoc . bsonifyValue . A.toJSON
  where getDoc (Doc d) = d

bson2obj :: (A.FromJSON a) => B.Document -> Maybe a
bson2obj bson = let res = A.fromJSON . aesonifyValue . Doc $ bson
                in case res of
                    A.Error _ -> Nothing
                    A.Success o -> Just o
