{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Stock.Article
       ( SearchOptions(..)
       , defaultOptions
       , findArticle
       , countArticles
       , findArticles
       , postArticle
       , updateArticle
       , getComments
       , postComment
       ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Char (toLower)
import qualified Data.List as L
import           Data.Maybe
import           Data.String.Conv
import           Database.MongoDB

--
import           Stock.Config
import           Stock.MongoDB
import           Stock.Types
import           Stock.Random
import           Stock.Tags
import qualified Stock.Markdown as MD

----------------------------------------------------------------------
-- debug
import           System.IO.Unsafe
import           Debug.Trace

--d = do
--  conf <- loadConfig "config.json"
--  res <- runMongo conf $ findArticles conf Nothing Nothing Public Nothing (Just ["Ruby"])
--  return res

----------------------------------------------------------------------

data SearchOptions = SearchOptions { optionsCursor :: Maybe Integer
                                   , optionsLimit :: Maybe Integer
                                   , optionsPeriod :: Maybe (UnixTime, UnixTime)
                                   , optionsTags :: Maybe [Tag]
                                   , optionsUser :: Maybe String
                                   }

defaultOptions = SearchOptions { optionsCursor = Nothing
                               , optionsLimit = Nothing
                               , optionsPeriod = Nothing
                               , optionsTags = Nothing
                               , optionsUser = Nothing
                               }

articleCollection = "articles"


findArticle :: (Functor m, MonadIO m) => String -> Action m (Maybe Article)
findArticle articleId = maybe Nothing bson2obj <$> findOne (select ["id" =: articleId] articleCollection)

optionToField :: ShowRegion -> SearchOptions -> IO [Field]
optionToField region opt = do
  mbtimestr <- liftIO $ getStrTimes (optionsPeriod opt)
  return $ selFields mbtimestr (optionsTags opt) (optionsUser opt)
  where
    selFields mbtime mbtags mbuser = selectWithTime mbtime ++ selectWithTags mbtags ++ selectWithUsers mbuser
                                     ++ selectWithRegion region
    getStrTimes (Just (start, end)) = do
      startStr <- timeToString start
      endStr <- timeToString end
      return $ Just (startStr, endStr)
    getStrTimes Nothing = return Nothing
    selectWithRegion Private = []
    selectWithRegion r = ["showRegion" =: (map toLower . show $ r)]
    selectWithTime (Just (start, end)) = ["timestamp" =: ["$gte" =: start, "$lte" =: end]]
    selectWithTime Nothing = []
    selectWithTags (Just tags) = ["tag" =: ["$in" =: tags]]
    selectWithTags Nothing = []
    selectWithUsers (Just userid) = ["authorId" =: userid]
    selectWithUsers Nothing = []


countArticles :: (Functor m, MonadIO m, MonadBaseControl IO m) =>
                Config -> ShowRegion -> SearchOptions -> Action m Int
countArticles conf region options = do
  fields <- liftIO $ optionToField region options
  res <- count (select fields articleCollection)
  return res

findArticles :: (Functor m, MonadIO m, MonadBaseControl IO m) =>
                Config ->
                ShowRegion -> SearchOptions -> Action m [Article]
findArticles conf region options = do
  fields <- liftIO $ optionToField region options
  catMaybes . map bson2obj
    <$> (rest =<< find (select fields articleCollection) { sort = ["timestamp" =: (-1 :: Int)]
                                                         , skip = cursor
                                                         , limit = limit })
  where
    cursor = maybe 0 fromIntegral (optionsCursor options)
    limit = maybe (fromIntegral . configTopArticlesNum $ conf) fromIntegral (optionsLimit options)

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
  sequence $ map (incrTagCount region) tags
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
                              , articleBody = MD.encode $ body
                              , articleBodyMarkdown = body
                              , articleTimestamp = ts
                              }

updateArticle :: (MonadIO m, Functor m) =>
                 ShowRegion -> String -> String -> [Tag] -> String -> Action m (Maybe Article)
updateArticle region articleId title tags body = do
  oldarticleM <- findArticle articleId
  let newarticle = maybe Nothing (\a -> Just a { articleTitle = title
                                               , articleTag = tags
                                               , articleBody = MD.encode $ body
                                               , articleBodyMarkdown = body
                                               , articleShowRegion = region
                                               }) oldarticleM
  maybe (return Nothing) (\a -> do
                             replace (select ["id" =: articleId] articleCollection) (obj2bson a)
                             sequence_ $ map (decrTagCount region) (articleTag . fromJust $ oldarticleM)
                             sequence_ $ map (incrTagCount region) (articleTag . fromJust $ newarticle)
                             return $ Just a) newarticle

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
