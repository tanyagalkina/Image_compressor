module ImgComp
    ( imgComp
     ,compManager
    ) where

import ReadPic
import KMeans
import Types
import Debug.Trace

compManager ::(Int, Float, [Pixel]) -> Int -> [Cluster]
--compManger (nb,l, pic) ran | trace ("manager " ++ show ran ++ " " ++ show nb) False = undefined
compManager (_, _, []) _ = []
compManager (1, _, pic) _ = 
    [Cluster {mean = calcMean $ map clr pic, pix = pic}]
compManager (nb,l, pic) ran = imgComp (len,lim, pic) set
         where set = (randomCentroids (map clr pic) nb ran)
               len = length set
               lim = (getMinDist set 1000000) * l 

constructRes :: [Color] -> [Cluster] -> [Cluster]-> [Cluster]
constructRes [] place [] = place
constructRes (r:rs) place (o:os) = constructRes rs 
    (Cluster {mean = r, pix = (pix o)}:place) os

imgComp :: (Int, Float,[Pixel]) -> [Color] -> [Cluster]
imgComp (nb, lim, pic) cenPrev  
              | checkMoves  cenPrev cenNext lim == False 
                  = imgComp (nb, lim, pic) cenNext  
              | otherwise = constructRes cenNext [] 
                  (placeChunks (kConstructor cenPrev []) pic)
  where cenNext = (kMeans $ placeChunks (kConstructor cenPrev []) pic)

checkMoves :: [Color] -> [Color] -> Float -> Bool
--checkMoves (x:xs) (y:ys) lim |
checkMoves [] [] lim = True
-- trace ("checkMoves  " ++ show lim ++ " " ++ show c) False = undefined
checkMoves (x:xs) (y:ys) lim | calcDist x y < lim = checkMoves xs ys lim
                             | otherwise = False

replaceNth :: Int -> Cluster -> [Cluster] -> [Cluster]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
      | n == 0 = newVal:xs
      | otherwise = x:replaceNth (n-1) newVal xs

constrNewCluster :: Pixel -> Cluster -> Cluster
constrNewCluster p clus = Cluster {mean = (mean clus), pix = p:(pix clus) }

placeChunks :: [Cluster] -> [Pixel] -> [Cluster]
--placeChunks build (p:ps) | trace ("place Chunks " ++ show (map mean build) ++ " " ) False = undefined
placeChunks build [] = build
placeChunks build (p:ps) = placeChunks (replaceNth ind newVal build) ps
                  where newVal = constrNewCluster p (build !! ind)
                        ind =  getChunkInd (clr p) build  1000000 0 0 

getChunkInd :: Color -> [Cluster] -> Float -> Int -> Int -> Int
getChunkInd _ [] _ _ minIdx = minIdx
getChunkInd line (cur : rest) curMin curIdx minIdx  | calcDist line 
    (mean cur) < curMin = getChunkInd line rest 
                     (calcDist line (mean cur)) (curIdx + 1) curIdx
    | otherwise = getChunkInd line rest curMin (curIdx + 1) minIdx    

kConstructor :: [Color] -> [Cluster] -> [Cluster]
kConstructor [] curr = reverse curr
kConstructor (c:cs) curr = 
    (kConstructor cs (Cluster {mean = c, pix = []}:curr))