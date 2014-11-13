{-# LANGUAGE OverloadedStrings #-}
module Stock.Markdown
       ( encode
       ) where

import           Data.List
import           Data.Maybe

-- debug
import           Control.Monad
import           Debug.Trace
import           Test.Hspec

-------------------- NEED TO REFACTORING
-------------------- INCOMPLETE

sample = "# SampleText\n\nHello, World\n\n+ Contents\n    - Apple\n    - Orange\n+ Other\n    - Foo\n    - Bar\n\nEnjoy!\n"
sample2 = "#Hello\nWorl"
sample3 = "#Hello\n- World\n-World?\n    + World\n- Yes\n yes fooo!!\n\nFoo"
sample4 = "\n\n\n"

data MValue = MValue [MValue]
            | MString String
            | MCode String
            | MHeader Int String
            | MDisc { mdiscLevel :: Int, mdiscValue :: [MValue] }
            | MDiscStr String
            deriving (Show, Eq)

data MParser = MParser { mparserValues :: [MValue], mparserLeft :: String }
             | MError String String
               deriving (Show, Eq)

encode = toMD . toMValue

toMD (MValue vals) = concat $ map toMD vals
toMD (MString str) = str ++ "<br />"
toMD (MHeader lv val) = concat ["<h", show lv, ">", val, "</h", show lv, ">"]
toMD (MDisc lv vals) = concat ["<ul>", concat $ map toMD vals, "</ul>"]
toMD (MDiscStr val) = concat ["<li>", val, "</li>"]

toMValue :: String -> MValue
toMValue str = MValue $ mparserValues . parse . parser $ str

parser :: String -> MParser
parser = MParser []

parse :: MParser -> MParser
parse ps@(MParser vals str)
  | str == [] = MParser (reverse vals) str
  | head str == '\n' = parse . skipChar $ ps
  | head str == '#' = parse . takeHeader $ ps
  | head str `elem` ['-', '+', '*'] = parse . takeDiscs $ ps
  | head str == ' ' = parse . skipChar $ ps
  | otherwise = parse . takeStrLn $ ps

skipChar :: MParser -> MParser
skipChar (MParser vals str) = MParser vals (tail str)

takeStrLn :: MParser -> MParser
takeStrLn (MParser vals str) = MParser (mstring:vals) (drop (length string) str)
  where string = takeWhile (/='\n') str
        mstring = MString string

takeHeader :: MParser -> MParser
takeHeader (MParser vals str) = MParser (header:vals) left
  where level = length $ takeWhile (=='#') str
        content = dropWhile (\ch -> ch == '#' || ch == ' ') . takeWhile (/='\n') $ str
        header = MHeader level content
        left = dropWhile (/='\n') str

takeDiscs :: MParser -> MParser
takeDiscs (MParser vals str) = MParser ( (parseDiscs 1 discLines) :vals) left
  where strLines = lines str
        discLines = takeWhile isDisc $ strLines
        left = concat $ drop (length discLines) strLines

isDisc :: String -> Bool
isDisc line = head (dropWhile (==' ') . takeWhile (/= '\n') $ line) `elem` ['-','+','*']

parseDiscs :: Int -> [String] -> MValue
parseDiscs level strs = MDisc level (fold 0 [] False)
  where disclevel line = (+1) $ (length . takeWhile (==' ') $ line) `div` 4
        cutValue :: String -> MValue
        cutValue line = let value = dropWhile (`elem` ['-','+','*',' ']) line
                        in MDiscStr value
        nextSamelevel cursor = let next = listToMaybe $
                                          filter (\(i, lv) -> lv == level && i > cursor) $ zip [0..] $ map disclevel strs
                               in maybe (length strs) fst next
        fold :: Int -> [MValue] -> Bool -> [MValue]
        fold cursor parsed inlower
          | cursor == length strs = parsed
          | otherwise = if disclevel (strs !! cursor) == level
                        then fold (cursor + 1) (parsed ++ [cutValue $ (strs !! cursor)]) False
                        else if inlower
                             then fold (cursor + 1) parsed False
                             else fold (cursor + 1)
                                  (parsed ++ (parseDiscs (level+1)
                                              (drop cursor $ take (nextSamelevel cursor) $ strs )):[]
                                  ) True


t = hspec spec

spec = do
  describe "parseDiscs" $ do
    it "parseDiscs" $ do
      parseDiscs 1 ["- Fruits", "    + Apple", "    + Orange", "- Animal", "    + Cat", "    + Dog"]
      `shouldBe` MDisc 1 [MDiscStr "Fruits", MDisc 2 [MDiscStr "Apple", MDiscStr "Orange"], MDiscStr "Animal"
                         ,MDisc 2 [MDiscStr "Cat", MDiscStr "Dog"]]
  describe "parse" $ do
    sequence_ $ map (\(input, expected) -> do
                      it (input ++ " -> " ++ show expected) $ do
                        parse (parser input) `shouldBe` (MParser expected "")) cases
cases =
  [("Hello, World", [MString "Hello, World"])
  ,(nl ["Hello", "World?"], [MString "Hello", MString "World?"])
  ,("# title", [MHeader 1 "title"])
  ,("## title", [MHeader 2 "title"])
  ,("### title", [MHeader 3 "title"])
  ,(nl ["# title", "content"], [MHeader 1 "title", MString "content"] )
  ,(nl ["# title", "content", "### title2"], [MHeader 1 "title", MString "content", MHeader 3 "title2"] )
  ,(nl ["- Apple","- Orange", "- Banana"], [MDisc 1 [MDiscStr "Apple",MDiscStr "Orange",MDiscStr "Banana"]])
  ,(nl ["- Fruits", "    + Apple", "    + Orange", "- Animal", "    + Cat", "    + Dog"],
    [MDisc 1 [MDiscStr "Fruits", MDisc 2 [MDiscStr "Apple", MDiscStr "Orange"], MDiscStr "Animal"
                         ,MDisc 2 [MDiscStr "Cat", MDiscStr "Dog"]]])
  ]
  where nl = concat . intersperse "\n" :: [String] -> String
