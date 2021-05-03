module ImgComp
    ( imageCompressor
      , Cluster
    ) where

import CmdArgs
import ReadPic

{--data Pixel = Pixel
    { loc  :: String
    , clr    :: [Int]
    } deriving (Eq,Ord,Show)--}

data Cluster = Cluster
    {mean ::[Int]
     ,pix :: [Pixel]
    } deriving (Eq, Ord, Show)

imageCompressor :: (Int, Float, [Pixel]) -> [String] -> [Pixel]
imageCompressor (c, l, []) [] = []  
imageCompressor (c, l, f) [] = f

