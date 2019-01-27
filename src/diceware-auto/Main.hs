{-
  This software was inspired by The Diceware Passphrase Home Page
  http://world.std.com/~reinhold/diceware.html
-}

import Control.Monad (replicateM, replicateM_)
import Data.IntMap ( size )
import Data.List (intercalate)
import System.Environment ( getArgs )
import System.FilePath ( (</>), (<.>), FilePath )
import Text.Printf ( printf )

import Diceware.Math ( calculateEntropy )
import Diceware.Random ( generateRandomDieRoll )
import Diceware.Words ( Dicemap, listToKeyInt, loadWordlist, lookupWord )


dicewareWordlistPath :: FilePath
-- FIXME This isn't going to live here forever
dicewareWordlistPath = "util" </> "resources" </> "diceware" <.> "wordlist" <.> "asc"
-- dicewareWordlistPath = "diceware.wordlist.asc"


defaultNumWords, defaultNumLines :: String
defaultNumWords = "8"
defaultNumLines = "30"


{-
  Randomly generate dice rolls and return the corresponding diceware
  word
-}
getWord :: Dicemap -> IO String
getWord dicemap = do
  rNums <- replicateM 5 generateRandomDieRoll
  let key = listToKeyInt rNums
  maybe
    (error $ "Unable to proceed because this doesn't map to any word: " ++ (show key))
    return $ lookupWord key dicemap


{-
  Pick lists of lists of diceware words given a number of words per line
  and a number of lines
-}
pickWords :: [String] -> IO ()

pickWords (numWords : [])            =
  pickWords [numWords, defaultNumLines]

pickWords (numWordsS : numLinesS : []) = do
  mapWordlist <- loadWordlist dicewareWordlistPath

  let wordListSize = size mapWordlist
  printf "Number of elements (words) in word list: %d\n" wordListSize
  let numWords = read numWordsS
  printf "Number of words per passphrase: %d\n" numWords
  let entropy = calculateEntropy (fromIntegral wordListSize) (fromIntegral numWords)
  printf "Bits of entropy: %.2f\n\n" (entropy :: Double)

  replicateM_ (read numLinesS) $ do
    -- Generate the words
    generatedWordList <- replicateM numWords $ getWord mapWordlist

    -- Print it formatted with interspersed spaces
    putStrLn . intercalate " " $ generatedWordList

pickWords _                          =
  pickWords [defaultNumWords, defaultNumLines]


main :: IO ()
main = getArgs >>= pickWords
