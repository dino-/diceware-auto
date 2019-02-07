{-# LANGUAGE QuasiQuotes #-}

module Options
  ( Options (..)
  , parseOptions
  )
  where

import Data.Version ( showVersion )
import Diceware.Words ( WordsSource (DicewareStock) )
import Options.Applicative
import Paths_diceware_auto ( version )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )


data Options = Options
  { optDump :: Bool
  , optLines :: Int
  , optWordsSource :: WordsSource
  , optVersion :: Bool
  , optWords :: Int
  }


parser :: Parser Options
parser = Options
  <$> switch
      (  long "dump"
      <> help "Dump the (English) default word list file to stdout"
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
      (  long "list"
      <> short 'L'
      <> help "Word list to choose from"
      <> showDefault
      <> value DicewareStock
      <> metavar "STR"
      )
  <*> switch
      (  long "version"
      <> help "Display version"
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
Against everyone's better judgement, this is an automated Diceware passphrase generator. This software simulates rolling 5 six-sided dice and chooses the corresponding word in a Diceware word list. Words selected this way can comprise a passphrase that's not hard to memorize and yet very difficult to discover with brute-force.

As of 2019-Jan it's recommended to use 96 bits of entropy for your most sensitive passphrases. For Diceware this translates into 8 words in length which is the default value for this program. Bits of entropy will be calculated and displayed during execution. For more information on password strength, see [this Wikipedia page](https://en.wikipedia.org/wiki/Password_strength)

This software is obviously less secure than rolling five 6-sided dice and looking up the words in a paper word list. But it's quite a bit more convenient. It does make some small attempt to be safer by using the cryptonite library to generate random values for die rolls.

For more information on the Diceware system of passphrase generation, please see the [Diceware Passphrase Home Page](http://world.std.com/~reinhold/diceware.html)  Here you can also find links to word list files in languages other than English. The standard English Diceware word list has been compiled into this software and can be printed to stdout if desired. Other word list files are supported as well via an option.

Version %s  Dino Morelli <dino@ui3.info>
|]
