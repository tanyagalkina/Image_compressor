module Main where

import System.Environment
import Data.List
import Data.Maybe
import System.Exit
import Text.Read
import Options.Applicative
import Data.Semigroup ((<>))


import PrintColors
import ImgComp
import ReadPic
import OneMoreTry
import CmdArgs
--import ArgModule

greet :: Sample -> IO ()
greet (Sample c l p) = print c >> print l >> putStrLn p 
greet _ = return ()

--data Sample = Sample
--  {colors :: Int
--   ,limit :: Float
--   ,path :: String
--  }

{--sample :: Parser Sample
sample = Sample 
 <$> option auto
      (short 'n'
      <> metavar "N"
      <> help "number of colors in the final image"
      )
 <*> option auto
      (short 'l'
      <> metavar "L"
      <> help "convergence limit"
      )
 <*> strOption
      (
          short 'f' 
          <> metavar "F"
          <> help "path to the file containing the colors of the pixels"
      )--}  

    
goAway :: String -> Int -> IO ()
goAway x 84 = do
    putStrLn x
    exitWith (ExitFailure 84)
goAway x 0 = do
    exitWith(ExitSuccess)
           

{--checkArgs :: Maybe Sample -> IO()
checkArgs Nothing = goAway "bad!" 84
checkSArgs _ = goAway "" 0--}

{--comfort :: ParserResult Sample -> [String] -> IO ()
comfort a (x:xs) | x == "-h" = do
                     handleParseResult a ; print "help"
                 | otherwise = checkArgs $ getParseResult a      
                 --}

simple :: Maybe Sample -> IO()
simple Nothing = goAway "Bad!" 84
simple _ =  goAway "HAHAHA!" 0 


main :: IO ()
main = do
    argv <- getArgs
    simple $ myParseArgs argv
    --checkArgs $ myPasreArgs argv

            


    
    {--comfort (execParserPure p opts argv) argv
        where
            opts = info (sample <**> helper) ( fullDesc
                <> progDesc "Print a greeting for TARGET"
                <> header "hello - a test for optparse-applicative" )
            p = prefs showHelpOnError--}

    
            
        



    --p = prefs showHelpOnEmpty
