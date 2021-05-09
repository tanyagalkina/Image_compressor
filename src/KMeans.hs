module KMeans
    (calcMean
     ,kMeans
     ,calcDist
     ,getMinDist
     ,randomCentroids
    ) where
        
import Data.List
import Types
import Debug.Trace

getMinDist :: [Color] -> Float -> Float
getMinDist  [] mind  = mind
getMinDist (Color r g b:Color rr gg bb:xs) mind =
     getMinDist (Color rr gg bb:xs) (min a mind)
           where a = calcDist (Color r g b) (Color rr gg bb)
getMinDist _ mind = mind

calcDist :: Color -> Color -> Float
calcDist a c | a == c = 0
             | otherwise = 
    sqrt (r*r + g*g + b*b)
    where
        r = red a - red c
        g = grn a - grn c
        b = blu a - blu c

calcMean :: [Color] -> Color
calcMean = go 0 (Color 0 0 0)
     where
  go l (Color r g b) (Color r' g' b' : cs) = 
      go (l + 1) (Color (r + r')(g + g') (b + b')) cs
  go l (Color r g b) [] = Color (r / l) (g / l) (b / l)

replaceCentroids :: Cluster -> Color
replaceCentroids clstr | length (pix clstr) == 0 = mean clstr
                        | otherwise = (calcMean $  (map clr (pix clstr)))

kMeans :: [Cluster] -> [Color]
kMeans clstr = map replaceCentroids clstr

randomCentroids :: [Color] -> Int -> Int ->[Color]
randomCentroids initial nb ran | ((length $ nub initial) - ran) < nb = 
    take nb (nub initial)
        | otherwise = take nb (drop (ran) (nub initial))
