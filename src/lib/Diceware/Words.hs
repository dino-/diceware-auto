module Diceware.Words
  where

import Control.Arrow ( (&&&) )
import Data.Char ( isDigit )
import qualified Data.Map as M
import System.FilePath ( FilePath )


type Dicemap = M.Map String String


{-
  Load the diceware word list file into a Map
-}
loadWordlist :: FilePath -> IO Dicemap
loadWordlist dicewareWordlistPath = do
  wordlistLines <- lines <$> readFile dicewareWordlistPath
  let parsed = map (takeWhile isDigit &&& tail . dropWhile isDigit) wordlistLines
  return $ M.fromList parsed


lookupWord :: String -> Dicemap -> Maybe String
lookupWord = M.lookup
