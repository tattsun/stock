{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stock.Scotty
       ( Status(..)
       , toStatus
       , htmlEscape
       , hamletPath
       ) where

import           Data.Aeson
import           Data.String.Conv
import           Data.String.Utils
import           Web.Scotty


data Status = Success | Token | DataDuplicated | Unauthorized deriving (Show, Eq)

toStatus :: Status -> String -> Value
toStatus status detail = object $ ["statusId" .= show status
                                  ,"detail" .= detail]

htmlEscape :: (SerializeString a) => a -> a
htmlEscape = fromString . escape . toString
  where escape s = replace ">" "&gt;"
                   . replace "<" "&lt;"
                   . replace "\"" "&quot;"
                   . replace "&" "&amp;" $ s

hamletPath page = concat ["views/", page, ".hamlet"]
