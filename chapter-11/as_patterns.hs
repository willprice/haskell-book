module AsPatterns where

import Data.Char

-- Return true iff all the values in the first list appear in the second list
isSubSeqOf :: (Eq a) => [a] -> [a] -> Bool
isSubSeqOf [] _ = True
isSubSeqOf _ [] = False
isSubSeqOf (x:xs) ys@(y:ys_tail)
  | x == y = isSubSeqOf xs ys
  | otherwise = isSubSeqOf (x:xs) ys_tail


capitalize :: String -> (String, String)
capitalize "" = ("", "")
capitalize word@(firstChar:tailChars) = (word, toUpper firstChar : tailChars)

capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map capitalize $ words sentence
