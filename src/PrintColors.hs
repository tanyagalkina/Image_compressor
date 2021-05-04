module PrintColors
    ( printColors
    ) where

import System.Exit 
import ImgComp
import Text.Printf 
import ReadPic    

printPix :: [Pixel] -> IO ()
printPix []      = return ()
printPix (w:ws)  = do 
            printf "(%d,%d) (%.f,%.f,%.f)\n" ((x w) :: Int) ((y w) :: Int) 
                ((red (clr w)) :: Float) ((grn (clr w)) :: Float)
                     ((blu (clr w)) :: Float)
            printPix ws

prCluster :: Cluster -> IO ()
prCluster c = 
    printf "--\n(%.f,%.f,%.f)\n-\n"  ((red (mean c)) :: Float) 
        ((grn (mean c)) :: Float) ((blu (mean c)) :: Float) >>
    printPix (pix c)

printColors :: [Cluster] -> IO [()]
printColors [] =  
    putStrLn "There was something wrong in the file" >> 
        exitWith (ExitFailure 84)
printColors cl = mapM prCluster cl 