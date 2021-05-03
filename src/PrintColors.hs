module PrintColors
    ( printColors
    ) where

import System.Exit 
import ReadPic       

printColors :: [Pixel] -> IO ()
printColors [] =  putStrLn "There was something wrong in the file" >> exitWith (ExitFailure 84)
printColors cont = print (x (last cont)) >> print (x (head cont))