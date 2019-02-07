{-# LANGUAGE OverloadedStrings #-}

module Diceware.Words
  ( Dicemap
  , WordsSource (..)
  , describeWordsSource
  , dumpList
  , listToKeyInt
  , loadWordlist
  , lookupWord

  -- Re-exported
  , size, unpack
  )
  where

import Control.Arrow ( (&&&), first )
import Control.Monad ( unless )
import Data.Char ( isDigit )
import Data.IntMap.Lazy ( IntMap, size )
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe ( fromJust, isJust )
import Data.Text ( Text, unpack )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read ( decimal )
import System.Directory ( doesFileExist )
import System.Exit ( die )
import System.FilePath ( FilePath )

import qualified Diceware.Wordlist.DicewareImproved as DicewareImproved
import qualified Diceware.Wordlist.DicewareStock as DicewareStock
import qualified Diceware.Wordlist.Eff as Eff


type Dicemap = IntMap Text


data WordsSource
  = DicewareImproved
  | DicewareStock
  | Eff
  | File FilePath

instance Show WordsSource where
  show DicewareImproved   = "improved"
  show DicewareStock      = "diceware"
  show Eff                = "eff"
  show (File path)        = path


instance Read WordsSource where
  readsPrec _ "improved"  = [(DicewareImproved, "")]
  readsPrec _ "diceware"  = [(DicewareStock, "")]
  readsPrec _ "eff"       = [(Eff, "")]
  readsPrec _ path        = [(File path, "")]


describeWordsSource :: WordsSource -> String
describeWordsSource DicewareImproved  = "Improved Diceware word list, English"
describeWordsSource DicewareStock     = "stock Diceware word list, English"
describeWordsSource Eff               = "EFF's long word list"
describeWordsSource (File path)       = "file " ++ path


loadWordlist :: WordsSource -> IO Dicemap
loadWordlist ws = parseWordlist <$> dumpList ws

dumpList :: WordsSource -> IO Text

dumpList DicewareImproved = return DicewareImproved.contents

dumpList DicewareStock = return DicewareStock.contents

dumpList Eff = return Eff.contents

dumpList (File dicewareWordlistPath) = do
  exists <- doesFileExist dicewareWordlistPath
  unless exists $ die $ "Can't continue because " ++
    dicewareWordlistPath ++ " does not exist"
  T.readFile dicewareWordlistPath


parseWordlist :: Text -> Dicemap
parseWordlist =
  IntMap.fromList
    -- 3. Remove any with left sides that didn't read successfully
    . map (first fromJust) . filter (isJust . fst)
    -- 2. Turn the left side string of each tuple into a (Maybe Int)
    . map (first readMay)
    -- 1. Separate each line into the digits string and the word
    . map (T.takeWhile isDigit &&& tailDef "" . T.dropWhile isDigit)
    . T.lines


{-
  Safe tail of a Data.Text with default value for the case of a null Text
-}

tailDef :: Text -> Text -> Text
tailDef def ""  = def
tailDef _   txt = T.tail txt


{-
  Safe read function for Text that evaluates to Nothing upon failure
-}

readMay :: Integral a => Text -> Maybe a
readMay txt = case decimal txt of
  Right (int, "") -> Just int
  _               -> Nothing


{-
  Performing the Data.Text.unpack here instead of in the caller.
-}

lookupWord :: IntMap.Key -> Dicemap -> Maybe String
lookupWord key dicemap = T.unpack <$> IntMap.lookup key dicemap


{-
  Turn a list like ([1,2,3,4,5] :: [Int]) into (12345 :: Int)
-}

listToKeyInt :: [Int] -> Int
listToKeyInt = listToKeyInt' 0 . reverse

listToKeyInt' :: Int -> [Int] -> Int
listToKeyInt' _    []       = 0
listToKeyInt' exp' (x : xs) = x * (10 ^ exp') + listToKeyInt' (exp' + 1) xs
