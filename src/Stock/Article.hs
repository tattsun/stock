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

getArticles :: ShowRegion -> Maybe (DateTime, DateTime) -> Maybe [Tag] -> Action m [Article]
getArticles region mbtime mbtags = undefined

postArticle :: ShowRegion -> String -> String -> String -> [Tag] -> String -> Action m Article
postArticle region title authorId authorName tags body = undefined

updateArticle :: String -> [Tag] -> String -> Action m Article
updateArticle title tags body = undefined

getComments :: String -> Action m [Comment]
getComments articleId = undefined

postComment :: String -> String -> String -> String -> Action m [Comment]
postComment authorId authorName body = undefined
