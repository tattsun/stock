{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Stock.Tags
       ( getTagCounts
       , incrTagCount
       , decrTagCount
       ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.List as L
import Data.Maybe
import Database.MongoDB

--
import Stock.Config
import Stock.MongoDB
import Stock.Types


----------------------------------------------------------------------

tagsCollection = "tags"

getTagCounts :: (MonadIO m, Functor m, MonadBaseControl IO m) => ShowRegion -> Action m [TagCount]
getTagCounts region = do
  tags <- tagSort region . catMaybes . map bson2obj <$> (rest =<< find (select [] tagsCollection))
  if region == Public
    then return $ filter (\t -> getRegionCount region t /= 0) $ tags
    else return $ map (\t -> t { tagCountPrivCount = tagCountPrivCount t + tagCountPubCount t} ) tags

regionField :: ShowRegion -> Label
regionField Public = "pubCount"
regionField _ = "privCount"

reverseRegion :: ShowRegion -> ShowRegion
reverseRegion Public = Private
reverseRegion Private = Public

getRegionCount :: ShowRegion -> TagCount -> Integer
getRegionCount Public = tagCountPubCount
getRegionCount _ = tagCountPrivCount

incrTagCount :: (MonadIO m, Functor m) => ShowRegion -> Tag -> Action m ()
incrTagCount region tag = if length tag == 0
                          then return ()
                          else upsert (select ["name" =: tag] tagsCollection)
                               ["$inc" =: [regionField region =: (1 :: Int)
                                          ,regionField (reverseRegion region) =: (0 :: Int)]]

decrTagCount :: (MonadIO m, Functor m) => ShowRegion -> Tag -> Action m ()
decrTagCount region tag = if length tag == 0
                          then return ()
                          else upsert (select ["name" =: tag] tagsCollection)
                               ["$inc" =: [regionField region =: (-1 :: Int)
                                          ,regionField (reverseRegion region) =: (0 :: Int)]]
