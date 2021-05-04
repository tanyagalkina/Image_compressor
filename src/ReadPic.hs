module ReadPic
    ( readPixels
    , Color (..)
    ,Pixel(..)
    ) where

import Data.List
import Data.Maybe
import Text.Read

data Color = Color
  { red :: Float
  , grn :: Float
  , blu :: Float
  } deriving (Eq,Ord,Show)

data Pixel = Pixel
    { x :: Int
     ,y :: Int 
     ,clr:: Color
     --,r :: Float
     --,g :: Float
     --,b :: Float
    } deriving (Eq,Ord,Show)

readPixels :: String -> [Pixel]
readPixels [] = []
readPixels f = getMaybe (map parseLine (removeEmptyLine(lines f)))

getMaybe :: [Maybe a] -> [a] 
getMaybe lst = case haveNothing of
    True -> []
    False -> (map fromJust lst)
  where
    haveNothing = any isNothing lst

removeEmptyLine :: [String] -> [String]
removeEmptyLine arr
        | hasEmptyLine arr  = delete "" arr
        | otherwise             = arr

hasEmptyLine :: [String] -> Bool
hasEmptyLine []     = False
hasEmptyLine (x:xs)
        | null x        = True
        | otherwise     = hasEmptyLine xs

parseLine :: String -> Maybe Pixel
parseLine ln = buildPixel $ concat [(takeLoc (words ln !! 0)), 
    (takeClr (words ln !! 1))]

isValidPos :: Int -> Int -> Bool
isValidPos x y | x < 0 || y < 0 = False
               |otherwise = True 

isValidColor :: Int -> Int -> Int -> Bool    
isValidColor r g b | r < 0 || r > 255 || g < 0 ||  
     g > 255 || b < 0 || b > 255 = False
                   | otherwise = True

buildPixel :: [Maybe Int] -> Maybe Pixel
buildPixel = go . fromMaybe [] . sequenceA
   where 
      go :: [Int] -> Maybe Pixel
      go (xa:ya:ra:ga:ba:_) | isValidPos xa ya && 
          isValidColor ra ga ba = Just Pixel {x = xa, y = ya, 
      clr = Color { red = fromIntegral (ra), grn = 
          fromIntegral (ga), blu = fromIntegral (ba)}}
      go _ = Nothing

takeLoc :: String -> [Maybe Int]
takeLoc wd = readLoc (x, y)
           where x = (words (map putSpace wd)) !! 0
                 y = (words (map putSpace wd)) !! 1

takeClr :: String -> [Maybe Int]
takeClr wd = readClr (r, g, b)
              where r = (words (map putSpace wd)) !! 0
                    g = (words (map putSpace wd)) !! 1
                    b = (words (map putSpace wd)) !! 2

readClr :: ([Char], [Char], [Char]) -> [Maybe Int]
readClr (r, g, b) = [(readMaybe r :: Maybe Int), 
    (readMaybe g :: Maybe Int), (readMaybe b :: Maybe Int)]

readLoc :: ([Char], [Char]) -> [Maybe Int]
readLoc (x, y) = [(readMaybe x :: Maybe Int), 
    (readMaybe y :: Maybe Int)]

putSpace :: Char -> Char
putSpace '('       = ' '
putSpace ')'       = ' '
putSpace ','       = ' '
putSpace c          = c