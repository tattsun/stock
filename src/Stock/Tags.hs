{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Stock.Tags
       ( getTagCounts
       , incrTagCount
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

getTagCounts :: (MonadIO m, Functor m, MonadBaseControl IO m) => Action m [TagCount]
getTagCounts = L.sort . catMaybes . map bson2obj <$> (rest =<< find (select [] tagsCollection))

incrTagCount :: (MonadIO m, Functor m) => Tag -> Action m ()
incrTagCount tag = upsert (select ["name" =: tag] tagsCollection) ["$inc" =: ["count" =: (1 :: Int)]]
