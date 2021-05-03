module Main where

import System.Environment
import Data.List
import Data.Maybe
import System.Exit
import Text.Read
import Data.String
import Data.Either
import Control.Exception
import Options.Applicative
import Data.Semigroup ((<>))


import PrintColors
import ImgComp
import ReadPic
import CmdArgs
           
usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
        putStrLn "\tN\tnumber of colors in the final image" >>
        putStrLn "\tL\tconvergence limit" >>
        putStrLn "\tF\tpath to the file containing the colors of the pixels"
        --goAway "" 0

getImage :: String -> [String]
getImage  input  = [input, input]

--goAway :: String -> Int -> IO ()
--goAway x 84 = putStrLn x >> exitWith (ExitFailure 84)
--goAway x 0 = exitWith(ExitSuccess)

manager :: Maybe Sample -> [String] -> IO ()
manager _ [] = print "give me something to do!" >> exitWith (ExitFailure 84)
manager Nothing ("-h":_) = usage >> exitWith (ExitSuccess)
manager Nothing ("--help": _) = usage >> exitWith (ExitSuccess)
manager Nothing (_:_)  = putStrLn "There were bad arguments" >> usage >> exitWith (ExitFailure 84)
manager (Just sam) (_:_)  = do
        res <- try $ readFile (path sam) :: IO (Either IOException String)
        case res of
            Left except -> print except >> exitWith (ExitFailure 84)
            Right f -> printColors $ imageCompressor (colors sam, limit sam, readPixels f) []

main :: IO ()
main = do
    argv <- getArgs
    manager (myParseArgs argv) argv