{-# LANGUAGE OverloadedStrings #-}
module Stock.Types where

----------------------------------------------------------------------
-- *** Article
type Tag = String

data Comment = Comment { commentId         :: String
                       , commentAuthorId   :: String
                       , commentAuthorName :: String
                       , commentBody       :: String
                       , commentTimestamp  :: String
                       } deriving (Show, Eq)

data Article = Article { articleId           :: String
                       , articleTitle        :: String
                       , articleAuthorId     :: String
                       , articleAuthorName   :: String
                       , articleTag          :: [Tag]
                       , articleBody         :: String
                       , articleStockUserIds :: [String]
                       , articleComments     :: [Comment]
                       , articleLikeUserIds  :: [String]
                       , articleTimestamp    :: String
                       } deriving (Show, Eq)

-------------------------------------------------------------------------------
-- *** User
data User = User { userId              :: String
                 , userPassword        :: String
                 , userProfile         :: String
                 , userStockArticleIds :: [String]
                 } deriving (Show, Eq)
