{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits inputNum =
  toDigitsHelper inputNum []

toDigitsHelper :: Integer -> [Integer] -> [Integer]
toDigitsHelper 0 currOutput =
  currOutput
toDigitsHelper num currOutput =
  toDigitsHelper (div num 10) ((mod num 10) : currOutput)

toDigitsRev :: Integer -> [Integer]
toDigitsRev inputNum =
  reverse (toDigits inputNum)

main :: IO()
main = print (toDigitsRev 123456789)