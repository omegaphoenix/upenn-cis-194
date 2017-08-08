module HW1Spec
  ( spec
  ) where

import HW1
import Test.Hspec

spec :: Spec
spec = do
  exercise1
  exercise2
  exercise3
  exercise4
  exercise5
  exercise6

exercise1 :: Spec
exercise1 =
  describe "toDigits" $ do
    it "Correctly handles digits" $ toDigits 1234 `shouldBe` [1, 2, 3, 4]
    it "Correctly reverse digits" $ toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
    it "Handles 0" $ toDigits 0 `shouldBe` []
    it "Handles negatives" $ toDigits (-17) `shouldBe` []
    it "Handles negatives reverse case" $ toDigitsRev (-188857) `shouldBe` []

exercise2 :: Spec
exercise2 =
  describe "doubleEveryOther" $ do
    it "doubles an even length list" $ doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
    it "doubles another even length list" $ doubleEveryOther [1, 3, 8, 6] `shouldBe` [2, 3, 16, 6]
    it "doubles an odd length list" $ doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]
    it "doubles another odd length list" $ doubleEveryOther [1, 2, 3, 5, 4] `shouldBe` [1, 4, 3, 10, 4]
    it "doubles odd length list with negatives" $ doubleEveryOther [-1, 2, 3, -5, 4] `shouldBe` [-1, 4, 3, -10, 4]

exercise3 :: Spec
exercise3 = describe "sumDigits" $ do
  it "sums with tens" $ sumDigits [16, 7, 12, 5] `shouldBe` 22
  it "sums with tens" $ sumDigits [2, 3, 16, 6] `shouldBe` 18
  it "sums empty list" $ sumDigits [] `shouldBe` 0
  it "sums single element" $ sumDigits [145] `shouldBe` 10
  it "sums a bunch of numbers" $ sumDigits [1..10] `shouldBe` 5*9 + 1
  it "handles negative number" $ sumDigits [-145] `shouldBe` 10
  it "handles negative numbers" $ sumDigits [-1, -2, 3] `shouldBe` 6

baseCard = 4012888888881881

exercise4 :: Spec
exercise4 =
  describe "validate" $ do
    it ("validates   " ++ show baseCard) $ validate baseCard `shouldBe` True
    it ("invalidates " ++ (show . (+ 1)) baseCard) $ validate (baseCard + 1) `shouldBe` False

exercise5 :: Spec
exercise5 =
  describe "hanoi" $ do
    it "Solves 1" $ hanoi 1 "x" "y" "z" `shouldBe` [("x", "z")]
    it "Solves 2" $ hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
    it "Solves 15 in minimum number of moves" $ length (hanoi 15 "a" "b" "c") `shouldBe` 32767

exercise6 :: Spec
exercise6 =
  describe "hanoi four pegs" $ do
    it "Solves 1" $ hanoiFour 1 "w" "x" "y" "z" `shouldBe` [("w", "z")]
    it "Solves 2" $ length (hanoiFour 2 "a" "b" "c", "d") `shouldBe` 3
    it "Solves 15 in minimum number of moves" $ length (hanoiFour 15 "a" "b" "c" "d") `shouldBe` 129
