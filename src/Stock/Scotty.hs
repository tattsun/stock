{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stock.Scotty
       ( Status(..)
       , toStatus
       , htmlEscape
       , hamletPath
       , paramDefault
       , paramMaybe
       ) where

import           Control.Applicative
import           Control.Exception
import           Data.Aeson
import           Data.String.Conv
import           Data.String.Utils
import qualified Data.Text.Lazy      as TL
import           Web.Scotty


data Status = Success | Token | DataDuplicated | Unauthorized | Failed deriving (Show, Eq)

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

paramDefault :: (Parsable a) => TL.Text -> a -> ActionM a
paramDefault p def = param p `rescue` (\_ -> return def)

paramMaybe :: (Parsable a) => TL.Text -> ActionM (Maybe a)
paramMaybe p = (Just <$> param p) `rescue` (\_ -> return Nothing)
