module CmdArgs
    ( myTestingFunc
    ,myParseArgs
    ,Sample(..)
    ) where

import Options.Applicative
import Data.Semigroup ((<>))

myTestingFunc :: [String] -> IO()
myTestingFunc (x:xs) = putStrLn x


greet :: Sample -> IO ()
greet (Sample c l p) = print c >> print l >> putStrLn p 
greet _ = return ()

data Sample = Sample
  {colors :: Int
   ,limit :: Float
   ,path :: String
  }

sample :: Parser Sample
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
      )

checkArgs :: Maybe Sample -> Maybe Sample
checkArgs Nothing = Nothing
checkArgs (Just sam) | colors sam < 1  = Nothing
                     | limit sam < 0 = Nothing
                     | limit sam > 1 = Nothing
                     | path sam == "" = Nothing
                     | otherwise = (Just sam)



                  


--comfort :: ParserResult Sample -> [String] -> Maybe a
--comfort a ("-h":xs) |  handleParseResult  return Nothing
--comfort a _         |  getParseResult a  
                    -- handleParseResult a ; Nothing
 --                   | otherwise = checkArgs $ getParseResult a    

umleitung :: Maybe Sample
umleitung = Nothing

between :: ParserResult Sample -> [String] -> Maybe Sample
--between  sam ("-h":_) = return $ handleParseResult sam 
                       -- umleting ()
between sam _ = getParseResult sam

myParseArgs :: [String] -> Maybe Sample
myParseArgs argv = checkArgs $ (between  (execParserPure p opts argv) argv)
    where
  opts = info (sample <**> helper) ( fullDesc
                <> progDesc "Hello, my dear"
                <> header "What do u want to know??" )
  p = prefs showHelpOnError

--parseArgs ("-h":xs) = handleParseResult --}

{--parseArgs argv = comfort (execParserPure p opts argv) argv
    where
  opts = info (sample <**> helper) ( fullDesc
                <> progDesc "Print a greeting for TARGET"
                <> header "What do u want to know??" )
  p = prefs showHelpOnError--}

{--data Options = Options
{optHelp :: Bool
, optVersion :: Bool
, optColors :: Int
, optLimit :: Float
, optPath :: String



--}
{--deriving (Show)

parseArgs :: [String]
parseArgs :: putStrLn "I am parsing the args" 
{--data Opts = Opts
{ _colors :: Int
  , _limit :: Float
  , _path :: String
 --}

options :: Parser Opts
options = 
    Opts 
    <$> IntOption
    (
        short 'n'
        <> mevatar "N"
        <> help "number of oclors in the final image"
    )
    $> FloatOption
    (
        short 'l'
        <> mevatar "L"
        <> help "convergence limit"
    )


  where
      colors :: Parser Int
      colors = flag N $
      short "-n'"
      <> help "Number of colors in the final image"--}

    {--filename :: Parser [String]
    filename = many $ argument str $
         metavar "filename..."
      <> help "Input files"

    fast :: Parser Speed
    fast = flag Slow Fast $
         long "cheetah"
      <> help "Perform task quickly."

    quiet :: Parser Bool
    quiet = switch $
         long "quiet"
      <> help "Whether to shut up."




     (
         <> short '-n'
     )--}



{--parseArgs :: [String] -> IO ()
parseArgs _ = putStrLn "I am parse Args"


usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"

exitError :: IO a
exitError = exitWith (ExitFailure 84)

exit :: IO Int
exit = exitWith ExitSuccess--}