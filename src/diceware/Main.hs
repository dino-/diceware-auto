{-
  This software was inspired by The Diceware Passphrase Home Page
  http://world.std.com/~reinhold/diceware.html
-}

import Control.Monad (replicateM, replicateM_)
import Data.List (intercalate)
import System.Environment ( getArgs )
import System.FilePath ( (</>), (<.>), FilePath )
import System.Random ( newStdGen, randomRs )
import Text.Printf ( printf )

import Diceware.Words


dicewareWordlistPath :: FilePath
-- FIXME This isn't going to live here forever
dicewareWordlistPath = "util" </> "resources" </> "diceware" <.> "wordlist" <.> "asc"
-- dicewareWordlistPath = "diceware.wordlist.asc"


defaultNumWords, defaultNumLines :: String
defaultNumWords = "6"
defaultNumLines = "20"


{-
  Randomly generate dice rolls and return the corresponding diceware
  word
-}
getWord :: Dicemap -> IO String
getWord dicemap = do
  g <- newStdGen
  let rNums = take 5 $ randomRs (1, 6 :: Int) g
  let key = concat $ map show rNums
  maybe
    (error $ "Unable to proceed becuase this doesn't map to any word: " ++ key)
    return $ lookupWord key dicemap


{-
  Pick lists of lists of diceware words given a number of words per line
  and a number of lines
-}
pickWords :: [String] -> IO ()

pickWords (numWords : [])            =
  pickWords [numWords, defaultNumLines]

pickWords (numWords : numLines : []) = do
  mapWordlist <- loadWordlist dicewareWordlistPath

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
