import Control.Monad (replicateM, replicateM_)
import Data.List (intercalate)
import System.FilePath ( (</>), (<.>), FilePath )
import Text.Printf ( printf )

import Diceware.Math ( calculateEntropy )
import Diceware.Random ( generateRandomDieRoll )
import Diceware.Words ( Dicemap, size, listToKeyInt, loadWordlist, lookupWord )
import Options


dicewareWordlistPath :: FilePath
-- FIXME This isn't going to live here forever
dicewareWordlistPath = "util" </> "resources" </> "diceware" <.> "wordlist" <.> "asc"
-- dicewareWordlistPath = "diceware.wordlist.asc"


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
pickWords :: Options -> IO ()
pickWords opts = do
  mapWordlist <- loadWordlist dicewareWordlistPath

  let wordListSize = size mapWordlist
  printf "Number of elements (words) in word list: %d\n" wordListSize
  let words' = optWords opts
  printf "Number of words per passphrase: %d\n" words'
  let entropy = calculateEntropy (fromIntegral wordListSize) (fromIntegral words')
  printf "Bits of entropy: %.2f\n\n" (entropy :: Double)

  replicateM_ (optLines opts) $ do
    -- Generate the words
    generatedWordList <- replicateM words' $ getWord mapWordlist

    -- Print it formatted with interspersed spaces
    putStrLn . intercalate " " $ generatedWordList


main :: IO ()
main = parseOptions >>= pickWords
