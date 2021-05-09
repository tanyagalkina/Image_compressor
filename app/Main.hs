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
import Types
import System.Random
           
usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
        putStrLn "\tN\tnumber of colors in the final image" >>
        putStrLn "\tL\tconvergence limit" >>
        putStrLn "\tF\tpath to the file containing the colors of the pixels"

getImage :: String -> [String]
getImage  input  = [input, input]

manager :: Maybe Sample -> [String] -> IO [()]
manager _ [] = print "give me something to do!" >> exitWith (ExitFailure 84)
manager Nothing ("-h":_) = usage >> exitWith (ExitSuccess)
manager Nothing ("--help": _) = usage >> exitWith (ExitSuccess)
manager Nothing (_:_)  = putStrLn "There were bad arguments" >> usage 
    >> exitWith (ExitFailure 84)
manager (Just sam) (_:_)  = do
        myran <- randomRIO (0, colors sam)
        res <- try $ readFile (path sam) :: IO (Either IOException String)
        case res of
            Left except -> print except >> exitWith (ExitFailure 84)
            Right f -> printColors $ 
                compManager (colors sam, limit sam, readPixels f) myran

main :: IO [()]
main = do
    argv <- getArgs
    manager (myParseArgs argv) argv