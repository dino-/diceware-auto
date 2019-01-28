{-# LANGUAGE QuasiQuotes #-}

module Options
  ( Options (..)
  , parseOptions
  )
  where

import Data.Version ( showVersion )
import Diceware.Words ( WordsSource (Internal, File) )
import Options.Applicative
import Paths_diceware_auto ( version )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )


data Options = Options
  { optWordsSource :: WordsSource
  , optLines :: Int
  , optWords :: Int
  }


parser :: Parser Options
parser = Options
  <$> option (File <$> str)
      (  long "words-file"
      <> short 'f'
      <> help "Path to a file of diceware words"
      <> metavar "PATH"
      <> value Internal
      )
  <*> option auto
      (  long "lines"
      <> short 'l'
      <> help "Number of lines of words to generate"
      <> showDefault
      <> value 30
      <> metavar "INT"
      )
  <*> option auto
      (  long "words"
      <> short 'w'
      <> help "Number of words to generate per line"
      <> showDefault
      <> value 8
      <> metavar "INT"
      )


parseOptions :: IO Options
parseOptions = execParser $ info (parser <**> helper)
  (  header "diceware-auto - Randomly generate Diceware passphrases"
  <> footer'
  )


footer' :: InfoMod a
footer' = footerDoc . Just . string $ printf content (showVersion version)
  where content = tail . init $ [here|
As of 2019-Jan it's recommended to use 96 bits of entropy for your most sensitive passphrases. For Diceware this translates into 8 words in length which is the default value at this time. Bits of entropy will be calculated and displayed during execution. For more information on password strength, see https://en.wikipedia.org/wiki/Password_strength

This software is by-nature less secure than rolling five 6-sided dice and looking up the words in a paper word list. But it's quite a bit more convenient. It does make some small attempt to be safe by using the cryptonite library to generate random values for die rolls.

For more information on the Diceware system of passphrase generation, please see The Diceware Passphrase Home Page http://world.std.com/~reinhold/diceware.html  Here you can also find links to word list files in languages other than English.

Version %s  Dino Morelli <dino@ui3.info>
|]
