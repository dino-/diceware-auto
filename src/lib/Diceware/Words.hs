module Diceware.Words
  ( Dicemap
  , listToKeyInt
  , loadWordlist
  , lookupWord
  )
  where

import Control.Arrow ( (&&&), first )
import Data.Char ( isDigit )
import Data.IntMap.Lazy ( IntMap )
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe ( fromJust, isJust )
import Safe ( readMay, tailDef )
import System.FilePath ( FilePath )


type Dicemap = IntMap String


loadWordlist :: FilePath -> IO Dicemap
loadWordlist dicewareWordlistPath = do
  wordlistLines <- lines <$> readFile dicewareWordlistPath
  let parsed =
        -- 3. Remove any with left sides that didn't read successfully
        map (first fromJust) . filter (isJust . fst)
        -- 2. Turn the left side string of each tuple into a (Maybe Int)
        . map (first readMay)
        -- 1. Separate each line into the digits string and the word
        . map (takeWhile isDigit &&& tailDef "" . dropWhile isDigit)
        $ wordlistLines
  return $ IntMap.fromList parsed


lookupWord :: IntMap.Key -> Dicemap -> Maybe String
lookupWord = IntMap.lookup


listToKeyInt :: [Int] -> Int
listToKeyInt = listToKeyInt' 0 . reverse

listToKeyInt' :: Int -> [Int] -> Int
listToKeyInt' _    []       = 0
listToKeyInt' exp' (x : xs) = x * (10 ^ exp') + listToKeyInt' (exp' + 1) xs
