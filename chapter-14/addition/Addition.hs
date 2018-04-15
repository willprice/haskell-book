module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

multiply :: (Eq a, Num a) => a -> a -> a
multiply a 0 = 0
multiply a 1 = a
multiply a b = a + multiply a (b - 1)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
    describe "Multiplication" $ do
        it "1 x 0 = 0" $ do
            multiply 1 0 `shouldBe` 0
        it "1 x 1 = 1" $ do
            multiply 1 1 `shouldBe` 1
        it "2 x 1 = 2" $ do
            multiply 2 1 `shouldBe` 2
        it "1 x 2 = 2" $ do
            multiply 1 2 `shouldBe` 2
