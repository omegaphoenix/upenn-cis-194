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
toDigits inputNum =
  case inputNum > 0 of
    True ->
      toDigitsHelper inputNum []
    False ->
      []

toDigitsHelper :: Integer -> [Integer] -> [Integer]
toDigitsHelper 0 currOutput =
  currOutput
toDigitsHelper num currOutput =
  toDigitsHelper (div num 10) ((mod num 10) : currOutput)

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
  (mod (sumDigits (doubleEveryOther (toDigits inputNum))) 10) == 0

-- Exercise 5
-- Name of peg
type Peg = String
-- A move is moving the top disk from first peg to second peg
type Move = (Peg, Peg)

-- Return list of moves to solve towers of hanoi
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 peg1 _ peg3 =
  [(peg1, peg3)]
hanoi numDisks peg1 peg2 peg3 =
  (hanoi (numDisks - 1) peg1 peg2 peg3)
  ++ (moveDisks 1 peg1 peg2)
  ++ (hanoi (numDisks - 1) peg3 peg1 peg2)

-- Move n disks
moveDisks :: Integer -> Peg -> Peg -> [Move]
moveDisks 0 _ _ =
  []
moveDisks n peg1 peg2 =
  (peg1, peg2) : (moveDisks (n - 1) peg1 peg2)

-- Exercise 6
hanoiFour :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoiFour 1 peg1 _ _ peg4 =
  [(peg1, peg4)]
hanoiFour numDisks peg1 peg2 peg3 _ =
  (hanoi (numDisks - 1) peg1 peg2 peg3)
  ++ (moveDisks 1 peg1 peg2)
  ++ (hanoi (numDisks - 1) peg3 peg1 peg2)