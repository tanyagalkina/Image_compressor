module KMeans
    (calcMean
    , distanceF
    ) where
        
import Data.List
import Data.Maybe
import Types

calcMean :: [Color] -> Color
calcMean = go 0 (Color 0 0 0)
     where
  go l (Color r g b) (Color r' g' b' : cs) = 
      go (l + 1) (Color (r + r')(g + g') (b + b')) cs
  go l (Color r g b) [] = Color (r / l) (g / l) (b / l)

distance :: [Int] -> [Float] -> Float
distance x y = sqrt (x'*x' + y'*y' + z'*z')
    where
        x' = fromIntegral (x !! 0) - y !! 0
        y' = fromIntegral (x !! 1) - y !! 1
        z' = fromIntegral (x !! 2) - y !! 2

distanceF :: [Float] -> [Float] -> Float
distanceF x y = sqrt (x'*x' + y'*y' + z'*z')
    where
        x' = x !! 0 - y !! 0
        y' = x !! 1 - y !! 1
        z' = x !! 2 - y !! 2

{--mean :: [Pixel] -> Cluster
mean pixel = Cluster { pos = [ sum r / y',  sum g / y',  sum b / y'] }
        where
            r = [fromIntegral (color (pixel !! i) !! 0) | i <- take y [0,1..]]
            g = [fromIntegral (color (pixel !! i) !! 1) | i <- take y [0,1..]]
            b = [fromIntegral (color (pixel !! i) !! 2) | i <- take y [0,1..]]
            y = length pixel
            y' = fromIntegral (y)

applyKmean :: [Pixel] -> [Cluster] -> [Clustering]
applyKmean a b =  linkKneighbor a b

linkKneighbor :: [Pixel] -> [Cluster] -> [Clustering]
linkKneighbor img clusterList = [createClustering img clusterList i | i <- take (length clusterList) [0,1..]]

createClustering :: [Pixel] -> [Cluster] -> Int -> Clustering
createClustering img list x = Clustering { cluster = if length choosenPixel > 0 then mean choosenPixel else list !! x, pixels = choosenPixel}
            where choosenPixel = findPixel img list x

findPixel :: [Pixel] -> [Cluster] -> Int -> [Pixel]
findPixel img list counter = [img !! i | i <- take (length img) [0,1..], minimalDist (img !! i) list == counter]

minimalDist :: Pixel -> [Cluster] -> Int
minimalDist pix list = findMinimum [distance (color pix) (pos (list !! i)) | i <-  take (length list) [0,1..]]

findMinimum :: [Float] -> Int
findMinimum x = n
        where
            n = case elemIndex y x of
                Nothing -> -1
                Just n  -> n
            y = minimum x--}