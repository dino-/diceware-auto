{-
  This software was inspired by The Diceware Passphrase Home Page
  http://world.std.com/~reinhold/diceware.html
-}

import Control.Arrow ( (&&&) )
import Control.Monad (replicateM, replicateM_)
import Data.Char ( isDigit )
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Prelude hiding ( lookup )
import System.Environment ( getArgs )
import System.FilePath ( (</>), (<.>), FilePath )
import System.Random ( newStdGen, randomRs )
import Text.Printf ( printf )


type Dicemap = M.Map String String


dicewareWordlistPath :: FilePath
-- FIXME This isn't going to live here forever
dicewareWordlistPath = "util" </> "resources" </> "diceware" <.> "wordlist" <.> "asc"
-- dicewareWordlistPath = "diceware.wordlist.asc"


defaultNumWords, defaultNumLines :: String
defaultNumWords = "6"
defaultNumLines = "20"


{- Load the diceware word list file into a Map
-}
loadWordlist :: IO Dicemap
loadWordlist = do
  wordlistLines <- lines <$> readFile dicewareWordlistPath
  let parsed = map (takeWhile isDigit &&& tail . dropWhile isDigit) wordlistLines
  return $ M.fromList parsed


{-
  Randomly generate dice rolls and return the corresponding diceware
  word
-}
getWord :: Dicemap -> IO String
getWord dm = do
  g <- newStdGen
  let rNums = take 5 $ randomRs (1, 6 :: Int) g
  let key = concat $ map show rNums
  return $ fromJust $ M.lookup key dm


{- Pick lists of lists of diceware words given a number of words per line
   and a number of lines
-}
pickWords :: [String] -> IO ()

pickWords (numWords : [])            =
  pickWords [numWords, defaultNumLines]

pickWords (numWords : numLines : []) = do
  mapWordlist <- loadWordlist

  replicateM_ (read numLines) $ do
    -- Generate the words
    wordsLine <- replicateM (read numWords) $ getWord mapWordlist
    -- Calculate the length of the words only
    let nospaceLength = sum $ map length wordsLine
    -- Format a display string with interspersed spaces
    let displayString = intercalate " " wordsLine

    printf "%-55s |words: %d  chars: %d\n"
      displayString nospaceLength (length displayString)

pickWords _                          =
  pickWords [defaultNumWords, defaultNumLines]


main :: IO ()
main = getArgs >>= pickWords
