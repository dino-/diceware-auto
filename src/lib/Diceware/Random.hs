module Diceware.Random
  ( generateRandomDieRoll
  )
  where

import Crypto.Random ( getRandomBytes )
import qualified Data.ByteString as BS
import Data.Bits ( (.|.), shiftL )


generateRandomDieRoll :: IO Int
generateRandomDieRoll = do
  oneByte <- getRandomBytes 1
  let converted = (convertRange (0, 255) ((1::Double), 6)
        . fromBytes $ oneByte) :: Integer
  return . fromIntegral $ converted


fromBytes :: BS.ByteString -> Integer
fromBytes = BS.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b


convertRange :: (RealFrac r, Integral i1, Integral i2) =>
  (i2, i2) -> (r, r) -> i2 -> i1
convertRange (oldLow, oldHigh) (newLow, newHigh) input =
  round (
    (fromIntegral (input - oldLow) / fromIntegral (oldHigh - oldLow))
    * (newHigh - newLow) + newLow
    )
