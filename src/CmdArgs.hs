module CmdArgs
    (myParseArgs
    ,Sample(..)
    ) where

import Options.Applicative

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

myParseArgs :: [String] -> Maybe Sample
myParseArgs argv = checkArgs $ getParseResult (execParserPure p opts argv)
    where
  opts = info (sample <**> helper) ( fullDesc
                <> progDesc "Hello, my dear"
                <> header "What do u want to know??" )
  p = prefs showHelpOnError