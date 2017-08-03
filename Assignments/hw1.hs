{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
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

-- Exercise 2
-- Returns list with every other number doubled beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther inputDigits =
  doubleEveryOtherHelper (reverse inputDigits)

-- Returns reversed list with every other number doubled
doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] =
  []
doubleEveryOtherHelper (x:[]) =
  [x]
doubleEveryOtherHelper (x:(y:zs)) =
  (doubleEveryOtherHelper zs) ++ ((2 * y) : [x])

main :: IO()
main = print (doubleEveryOther (toDigits 12345678))
