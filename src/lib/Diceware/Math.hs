module Diceware.Math
  ( calculateEntropy
  )
  where


calculateEntropy :: Floating a => a -> a -> a
calculateEntropy numberOfElements lengthOfPassphrase =
  (logBase 2 numberOfElements) * lengthOfPassphrase
