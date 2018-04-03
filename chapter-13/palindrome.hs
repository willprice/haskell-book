import Control.Monad
import Data.Char


palindrome :: String -> Bool
palindrome l = l' == reverse l'
    where l' = filter isAlphaNum l

main :: IO ()
main = forever $ do
    line <- getLine
    if (palindrome line) 
       then putStrLn "It's a palindrome"
       else putStrLn "Not a palindrome"
