module Main where

import System.Environment
import Data.List
import Data.Maybe
import System.Exit
import Text.Read
import Data.String
import Data.Either
import Control.Exception
import System.Directory
import Options.Applicative
import Data.Semigroup ((<>))


import PrintColors
import ImgComp
import ReadPic
import OneMoreTry
import CmdArgs
           
usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
        putStrLn "\tN\tnumber of colors in the final image" >>
        putStrLn "\tL\tconvergence limit" >>
        putStrLn "\tF\tpath to the file containing the colors of the pixels" >>
        goAway "" 0

{--mymy :: String -> String
mymy path = do
    helloFile <- openFile path ReadMode
    hasLine <- hIsEOF helloFile
    firstLine <- if not hasLine
               then return readFile path 
    else return ""--}


getImage :: String -> [String]
getImage  input  = [input, input]

goAway :: String -> Int -> IO ()
goAway x 84 = putStrLn x >> exitWith (ExitFailure 84)
goAway x 0 = exitWith(ExitSuccess)

simple :: Maybe Sample -> [String] -> IO ()
simple _ [] = print "give me something to do!" >> exitWith (ExitFailure 84)
simple Nothing ("-h":_) = usage
simple Nothing ("--help": _) = usage
simple Nothing (_:_)  =  goAway "Bad!" 84
simple (Just sam) (_:_)  = do
        res <- try $ readFile (path sam) :: IO (Either IOException String)
        case res of
            Left except -> print except >> exitWith (ExitFailure 84)
            Right f -> printColors $ imageCompressor (colors sam, limit sam, getImage f) []

main :: IO ()
main = do
    argv <- getArgs
    simple (myParseArgs argv) argv