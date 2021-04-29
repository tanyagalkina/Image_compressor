module Main where

import System.Environment
import Data.List
import Data.Maybe
import System.Exit
import Text.Read
import System.Directory
import Options.Applicative
import Data.Semigroup ((<>))


import PrintColors
import ImgComp
import ReadPic
import OneMoreTry
import CmdArgs
--import ArgModule

goAway :: String -> Int -> IO ()
goAway x 84 = do
    putStrLn x
    exitWith (ExitFailure 84)
goAway x 0 = do
    exitWith(ExitSuccess)
           
usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
        putStrLn "\tN\tnumber of colors in the final image" >>
        putStrLn "\tL\tconvergence limit" >>
        putStrLn "\tF\tpath to the file containing the colors of the pixels" >>
        goAway "" 0


simple :: Maybe Sample -> [String] -> IO ()
simple Nothing ("-h":_) = usage
simple Nothing ("--help": _) = usage
simple Nothing (_:_)  =  goAway "Bad!" 84
simple (Just sam) (_:_) = do
     ex <- doesFileExist (path sam)
     if ex == True
         then print $ imageCompressor (Sample {colors = (colors sam), 
         limit = (limit sam), path = (path sam)})
     else goAway "Bad path" 84

main :: IO ()
main = do
    argv <- getArgs
    simple (myParseArgs argv) argv