{-# LANGUAGE QuasiQuotes #-}

module Options
  ( Options (..)
  , parseOptions
  )
  where

import Data.Version ( showVersion )
import Diceware.Words ( WordsSource (DicewareImproved) )
import Options.Applicative
import Paths_diceware_auto ( version )
import Text.Heredoc ( here )
import Text.PrettyPrint.ANSI.Leijen ( string )
import Text.Printf ( printf )


data Options = Options
  { optDump :: Bool
  , optLines :: Int
  , optWordsSource :: WordsSource
  , optCount :: Bool
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
      <> value 1
      <> metavar "INT"
      )
  <*> option auto
      (  long "list"
      <> short 'L'
      <> help "Word list to choose from. See WORD LISTS below."
      <> showDefault
      <> value DicewareImproved
      <> metavar "STR"
      )
  <*> flag True False
      (  long "no-count"
      <> short 'C'
      <> help "Don't display character count for each line"
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
Against everyone's better judgement, this is an automated Diceware passphrase
generator. This software simulates rolling 5 six-sided dice and chooses the
corresponding word in a Diceware word list. Words selected this way can
comprise a passphrase that's not hard to memorize and yet very difficult to
discover with brute-force.

As of 2019-Jan it's recommended to use 96 bits of entropy for your most
sensitive passphrases. Using a list of 7776 words, this translates into 8 words
in length which is the default value for this program. Bits of entropy will be
calculated and displayed during execution. For more information on password
strength, see
[this Wikipedia page](https://en.wikipedia.org/wiki/Password_strength)

This software is obviously less secure than rolling five 6-sided dice and
looking up the words in a paper word list. But it's quite a bit more
convenient. It does make some small attempt to be safer by using the cryptonite
library to generate random values for die rolls.

WORD LISTS

Three stock word lists have been compiled into this software and can be
retrieved to stdout with the `--dump` option. The possible choices for the
`-L|--list` switch are:

- improved: A combination of the original Diceware list plus the Beale list,
    minus many non-word things. This is the default list.
    [source](http://www.webplaces.org/passwords/diceware-criticism.htm)
- diceware: The original Diceware word list.
    [source](http://world.std.com/~reinhold/diceware.html)
- eff: The EFF's New Wordlists for Random Passphrases
    [source](https://www.eff.org/deeplinks/2016/07/new-wordlists-random-passphrases)

Any other value will be interpreted as a path to a word list file. These can be
found at some of the links above. Handy if you need a language other than
English.

Version %s  Dino Morelli <dino@ui3.info>
|]
