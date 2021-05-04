module ImgComp
    ( imageCompressor
      , Cluster (..)
    ) where

import CmdArgs
import ReadPic

data Cluster = Cluster
    {mean ::Color
     ,pix :: [Pixel]
    } deriving (Eq, Ord, Show)

imageCompressor :: (Int, Float, [Pixel]) -> [Cluster] -> [Cluster]
imageCompressor (c, l, []) [] = []
imageCompressor (1, _, f) [] = [Cluster {mean = calMean $ map clr f, pix = f}]  
imageCompressor (c, l, f) [] = [Cluster {mean = Color 
           9  4  3, pix = f}, 
      Cluster {mean = Color 56 23 83, pix = f}]

calMean :: [Color] -> Color
calMean = go 0 (Color 0 0 0)
     where
  go l (Color r g b) (Color r' g' b' : cs) = 
      go (l + 1) (Color (r + r')(g + g') (b + b')) cs
  go l (Color r g b) [] = Color (r / l) (g / l) (b / l)