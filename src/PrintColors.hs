module PrintColors
    ( printColors
    ) where

import System.Exit        

printColors :: [String] -> IO ()
printColors [[], []] =  putStrLn "The file was empty" >> exitWith (ExitFailure 84)
printColors cont = putStrLn $ head cont