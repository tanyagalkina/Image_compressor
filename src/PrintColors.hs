module PrintColors
    ( printColors
    ) where

printColors :: [String] -> IO ()
printColors cont = putStrLn $ head cont