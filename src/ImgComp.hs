module ImgComp
    ( imageCompressor
    ) where

import CmdArgs
--(colors sam, limit sam, getImage f) []
imageCompressor :: (Int, Float, [String]) -> [String] -> [String]
imageCompressor (c, l, [[], []]) [] = [[],[]]  

