{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stock.Scotty
       ( Status(..)
       , toStatus
       ) where

import           Data.Aeson
import           Web.Scotty

data Status = Success | Token | DataDuplicated | Unauthorized deriving (Show, Eq)

toStatus :: Status -> String -> Value
toStatus status detail = object $ ["statusId" .= show status
                                  ,"detail" .= detail]
