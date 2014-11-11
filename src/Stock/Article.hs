{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Stock.Article
       ( findArticle
       , findArticles
       , postArticle
       , updateArticle
       , getComments
       , postComment
       ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Maybe
import Database.MongoDB

--
import Stock.Config
import Stock.MongoDB
import Stock.Types
import Stock.Random

articleCollection = "articles"

findArticle :: (Functor m, MonadIO m) => String -> Action m (Maybe Article)
findArticle articleId = maybe Nothing bson2obj <$> findOne (select ["id" =: articleId] articleCollection)

findArticles :: (Functor m, MonadIO m, MonadBaseControl IO m) =>
                Config ->
                Maybe Integer -> Maybe Integer ->
                ShowRegion -> Maybe (UnixTime, UnixTime) -> Maybe [Tag] -> Action m [Article]
findArticles conf cursorM limitM region mbtime mbtags = do
  mbtimestr <- liftIO $ getStrTimes mbtime
  filter (\a -> articleShowRegion a == region) . catMaybes . map bson2obj
    <$> (rest =<< find (select (genFields mbtimestr mbtags) articleCollection) { sort = ["timestamp" =: (-1 :: Int)]
                                                                               , skip = cursor
                                                                               , limit = limit })
  where
    cursor = maybe 0 fromIntegral cursorM
    limit = maybe (fromIntegral . configTopArticlesNum $ conf) fromIntegral limitM
    genFields mbtime mbtags = selectWithTime mbtime ++ selectWithTags mbtags
    getStrTimes (Just (start, end)) = do
      startStr <- timeToString start
      endStr <- timeToString end
      return $ Just (startStr, endStr)
    getStrTimes Nothing = return Nothing
    selectWithTime (Just (start, end)) = ["timestamp" =: ["$gte" =: start, "$lte" =: end]]
    selectWithTime Nothing = []
    selectWithTags (Just tags) = ["tag" =: ["$in": tags]]
    selectWithTags Nothing = []

isExistsArticleId :: (Functor m, MonadIO m) => String -> Action m Bool
isExistsArticleId articleId = maybe False (const True) <$> findArticle articleId

getUnusedId :: (MonadIO m, Functor m) => Action m String
getUnusedId = do
  id <- liftIO $ randomStr 10
  isused <- isExistsArticleId id
  if isused
    then getUnusedId
    else return id

postArticle :: (MonadIO m, Functor m) =>
               ShowRegion -> String -> String -> String -> [Tag] -> String -> Action m Article
postArticle region title authorId authorName tags body = do
  article <- makearticle
  insert articleCollection (obj2bson article)
  return article
  where
    makearticle = do
      id <- getUnusedId
      ts <- liftIO $ getTimestamp
      return $ defaultArticle { articleId = id
                              , articleShowRegion = region
                              , articleTitle = title
                              , articleAuthorId = authorId
                              , articleAuthorName = authorName
                              , articleTag = tags
                              , articleBody = body
                              , articleTimestamp = ts
                              }

updateArticle :: (MonadIO m, Functor m) =>
                 String -> String -> [Tag] -> String -> Action m (Maybe Article)
updateArticle articleId title tags body = do
  newarticle <- maybe Nothing (\a -> Just a { articleTitle = title
                                            , articleTag = tags
                                            , articleBody = body
                                            }) <$> findArticle articleId
  maybe (return Nothing) (\a -> replace (select ["id" =: articleId] articleCollection) (obj2bson a)
                                >> (return $ Just a)) newarticle

getComments :: (MonadIO m, Functor m) => String -> Action m [Comment]
getComments articleId = do
  articleMaybe <- findArticle articleId
  return $ maybe [] articleComments articleMaybe

postComment :: (MonadIO m, Functor m) =>
               String -> String -> String -> String -> Action m (Maybe [Comment])
postComment articleId authorId authorName body = do
  comment <- makecomment
  article <- maybe Nothing (\a -> Just $ a { articleComments = comment : articleComments a }) <$> findArticle articleId
  maybe (return Nothing) (\a -> replace (select ["id" =: articleId] articleCollection) (obj2bson a)
                                >> return (Just $ articleComments a)) article
  where
    makecomment = do
      id <- liftIO $ randomStr 30
      ts <- liftIO $ getTimestamp
      return $ defaultComment { commentId = id
                              , commentAuthorId = authorId
                              , commentAuthorName = authorName
                              , commentBody = body
                              , commentTimestamp = ts
                              }
