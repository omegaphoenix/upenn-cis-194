{-# OPTIONS_GHC -Wall #-}
module HW1
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  , hanoi
  , hanoiFour
  ) where

-- Exercise 1
-- Convert integer to list of digits
toDigits :: Integer -> [Integer]
toDigits inputNum
  | inputNum > 0 = toDigitsHelper inputNum []
  | otherwise = []

toDigitsHelper :: Integer -> [Integer] -> [Integer]
toDigitsHelper num currOutput
  | num == 0 = currOutput
  | otherwise = toDigitsHelper (div num 10) ((mod num 10) : currOutput)

-- Convert integer to list of digits reversed
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

-- Exercise 3
-- Add up digits of list of integers
sumDigits :: [Integer] -> Integer
sumDigits [] =
  0
sumDigits (x:xs) =
  (sumDigitsHelper (toDigits (abs x))) + (sumDigits xs)

-- Add up integers
sumDigitsHelper :: [Integer] -> Integer
sumDigitsHelper [] =
  0
sumDigitsHelper (x:xs) =
  x + (sumDigitsHelper xs)

-- Exercise 4
-- Check if valid credit card number
validate :: Integer -> Bool
validate inputNum =
  inputNum >= 0
  && (mod (sumDigits (doubleEveryOther (toDigits inputNum))) 10) == 0

-- Exercise 5
-- Name of peg
type Peg = String
-- A move is moving the top disk from first peg to second peg
type Move = (Peg, Peg)

-- Return list of moves to solve towers of hanoi from src to dst
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numDisks src dst tmp
  | numDisks <= 0 = []
  | numDisks == 1 = moveDisk src dst
  | otherwise = (hanoi (numDisks - 1) src tmp dst)
              ++ moveDisk src dst
              ++ (hanoi (numDisks - 1) tmp dst src)

-- Move 1 disk from src to dst
moveDisk :: Peg -> Peg -> [Move]
moveDisk src dst =
  [(src, dst)]

-- Exercise 6
-- Solve Towers of Hanoi with 4 pegs in minimal number of moves
hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour numDisks src dst tmp1 tmp2
  | numDisks <= 0 = []
  | numDisks == 1 = [(src, dst)]
  | otherwise = hanoiFour k src tmp1 dst tmp2
              ++ hanoi (numDisks - k) src dst tmp2
              ++ hanoiFour k tmp1 dst src tmp2
              where k = numDisks - round(sqrt.fromInteger$(2 * numDisks + 1) :: Double) + 1