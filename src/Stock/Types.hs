{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stock.Types
       ( module Stock.Types
       , module Stock.Types.DateTime
       ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char

--
import           Stock.Types.DateTime
import           Stock.Types.Internal

----------------------------------------------------------------------
-- *** Config
data Config = Config { configServerPort      :: Int
                     , configPasswordSalt    :: String
                     , configMongoDBHostName :: String
                     , configMongoDBName     :: String
                     } deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = fieldFix 6} ''Config)


----------------------------------------------------------------------
-- *** Article
type Tag = String

data Comment = Comment { commentId         :: String
                       , commentAuthorId   :: String
                       , commentAuthorName :: String
                       , commentBody       :: String
                       , commentTimestamp  :: String
                       } deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = fieldFix 7} ''Comment)
defaultComment = Comment { commentId = ""
                         , commentAuthorId = ""
                         , commentAuthorName = ""
                         , commentBody = ""
                         , commentTimestamp = ""
                         }

data ShowRegion = All | Organization deriving (Show, Eq)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''ShowRegion)

data Article = Article { articleId           :: String
                       , articleShowRegion   :: ShowRegion
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
$(deriveJSON defaultOptions{fieldLabelModifier = fieldFix 7} ''Article)
defaultArticle = Article { articleId = ""
                         , articleShowRegion = All
                         , articleTitle = ""
                         , articleAuthorId = ""
                         , articleAuthorName = ""
                         , articleTag = ["未分類"]
                         , articleBody = ""
                         , articleStockUserIds = []
                         , articleComments = []
                         , articleLikeUserIds = []
                         , articleTimestamp = ""
                         }

-------------------------------------------------------------------------------
-- *** User
data User = User { userId              :: String
                 , userPassword        :: String
                 , userName            :: String
                 , userProfile         :: String
                 , userStockArticleIds :: [String]
                 , userTimestamp       :: String
                 } deriving (Show, Eq)
$(deriveJSON defaultOptions{fieldLabelModifier = fieldFix 4} ''User)
defaultUser = User { userId = ""
                   , userPassword = ""
                   , userName = ""
                   , userProfile = ""
                   , userStockArticleIds = []
                   , userTimestamp = ""
                   }
