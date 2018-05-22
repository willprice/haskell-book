module WordNumber where

import Data.List
import Data.Char

wordNumber :: Int -> String
wordNumber n =
    concat $ intersperse "-" (map digitToWord $ digits n)

digits :: Int -> [Int]
digits n = map (read . (\x -> [x])) (show n)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = "unknown"


