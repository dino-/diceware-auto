{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read ( decimal )
import System.FilePath ( FilePath )


type Dicemap = IntMap Text


loadWordlist :: FilePath -> IO Dicemap
loadWordlist dicewareWordlistPath = do
  wordlistLines <- T.lines <$> T.readFile dicewareWordlistPath
  let parsed =
        -- 3. Remove any with left sides that didn't read successfully
        map (first fromJust) . filter (isJust . fst)
        -- 2. Turn the left side string of each tuple into a (Maybe Int)
        . map (first readMay)
        -- 1. Separate each line into the digits string and the word
        . map (T.takeWhile isDigit &&& tailDef "" . T.dropWhile isDigit)
        $ wordlistLines
  return $ IntMap.fromList parsed


tailDef :: Text -> Text -> Text
tailDef def ""  = def
tailDef _   txt = T.tail txt


readMay :: Integral a => Text -> Maybe a
readMay txt = case decimal txt of
  Right (int, "") -> Just int
  _               -> Nothing


{-
  Performing the Data.Text.unpack here instead of in the caller.
-}

lookupWord :: IntMap.Key -> Dicemap -> Maybe String
lookupWord key dicemap = T.unpack <$> IntMap.lookup key dicemap


listToKeyInt :: [Int] -> Int
listToKeyInt = listToKeyInt' 0 . reverse

listToKeyInt' :: Int -> [Int] -> Int
listToKeyInt' _    []       = 0
listToKeyInt' exp' (x : xs) = x * (10 ^ exp') + listToKeyInt' (exp' + 1) xs
