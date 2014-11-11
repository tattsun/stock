{-# LANGUAGE OverloadedStrings #-}
module Stock.Types.Internal where

import           Data.Char

fieldFix :: Int -> String -> String
fieldFix prefixLen field = let (f:fs) = drop prefixLen field
                           in (toLower f):fs
