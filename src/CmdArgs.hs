module CmdArgs
    (myParseArgs
    ) where

import Options.Applicative
import Types

sample :: Parser Sample
sample = Sample 
 <$> option auto
      (short 'n'
      )
 <*> option auto
      (short 'l'
      )
 <*> strOption
      (
       short 'f' 
      )

checkArgs :: Maybe Sample -> Maybe Sample
checkArgs Nothing = Nothing
checkArgs (Just sam) 
          | colors sam < 1  = Nothing
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