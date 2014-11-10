{-# LANGUAGE OverloadedStrings #-}
module Stock.Article
       ( getArticle
       , postArticle
       , updateArticle
       , getComments
       , postComment
       ) where

import           Database.MongoDB
import           Stock.Config
import           Stock.Types

getArticle :: String -> Action m Article
getArticle articleId = undefined

getArticles :: ShowRegion -> Maybe (DateTime, DateTime) -> Maybe [Tag] -> IO [Article]
getArticles region mbtime mbtags = undefined

postArticle :: ShowRegion -> String -> String -> String -> [Tag] -> String -> IO Article
postArticle region title authorId authorName tags body = undefined

updateArticle :: String -> [Tag] -> String -> IO Article
updateArticle title tags body = undefined

getComments :: String -> IO [Comment]
getComments articleId = undefined

postComment :: String -> String -> String -> String -> IO [Comment]
postComment authorId authorName body = undefined
