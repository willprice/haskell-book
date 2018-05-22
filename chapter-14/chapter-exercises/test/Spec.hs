module Main where

import Test.Hspec
import WordNumber 

main :: IO ()
main = hspec $ do
    describe "digitToWord" $ do
        it "return zero for 0" $ do
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ do
            digitToWord 1 `shouldBe` "one"
        it "returns two for 2" $ do
            digitToWord 2 `shouldBe` "two"
        it "returns three for 3" $ do
            digitToWord 3 `shouldBe` "three"
        it "returns four for 4" $ do
            digitToWord 4 `shouldBe` "four"
        it "returns five for 5" $ do
            digitToWord 5 `shouldBe` "five"
        it "returns six for 6" $ do
            digitToWord 6 `shouldBe` "six"
        it "returns seven for 7" $ do
            digitToWord 7 `shouldBe` "seven"
        it "returns eight for 8" $ do
            digitToWord 8 `shouldBe` "eight"
        it "returns nine for 9" $ do
            digitToWord 9 `shouldBe` "nine"


    describe "digits" $ do
        it "returns [1] for 1" $ do
            digits 1 `shouldBe` [1]
        it "returns [1, 2, 3] for 123" $ do
            digits 123 `shouldBe` [1, 2, 3]

    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ do
            wordNumber 100 `shouldBe` "one-zero-zero"
