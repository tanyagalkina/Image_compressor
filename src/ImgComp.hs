module ImgComp
    ( imgComp
    ) where

import CmdArgs
import ReadPic
import KMeans
import Types

imgComp :: (Int, Float, [Pixel]) -> [Cluster] -> [Cluster]
imgComp (c, l, []) [] = []
imgComp (1, _, f) [] = [Cluster {mean = calcMean $ map clr f, pix = f}]  
imgComp (c, l, f) [] = [Cluster {mean = Color 
           9  4  3, pix = f}, 
      Cluster {mean = Color 56 23 83, pix = f}]



{--imgComp :: (Int, Float, [Pixel]) -> [Cluster] -> [Cluster]
imgComp (c, l, []) [] = []
imgComp (1, _, pic) [] = [Cluster {mean = calcMean $ map clr f, pix = f}]
imgComp (2, _, pic) [] = 
imgComp (c, l, pic) [] = imgComp (ca, l, pic) (kMeans pic ca [])
                where -- this fit is needed only for the first time
                ca = if c > len then len else c
                len = length pic      
imgComp (c, l, pic) last
        | ifLimited last new l == False = imgComp (c, l, pic) new
        | otherwise = new
            where
                new = kMeans pic c last

ifLimited :: [Cluster] -> [Cluster] -> Float -> Bool
ifLimited old new e
        | length x == length old    = True
        | otherwise                 = False
            where
                x = [function i | i <- take (length old) [0,1..] , function i == True]
                function i = eqCluster (cluster (old !! i)) (cluster (new !! i)) e


eqCluster :: Cluster -> Cluster -> Float -> Bool
eqCluster clusterA clusterB e
        | x > e     = False
        | otherwise = True
            where
                a = pos clusterA
                b = pos clusterB
                x = distanceF a b

newCluster :: [Pixel] -> Int -> [Cluster]
newCluster img n = [mean (y !! i) | i <- take n [0,1..]]
    where
        y = chunks (length img `div` n) img
        

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs--}
