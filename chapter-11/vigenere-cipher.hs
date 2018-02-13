module Ciphers where

import Data.Char


charToAlphaIndex :: Char -> Int
charToAlphaIndex c = ord c - ord 'a'

alphaIndexToChar :: Int -> Char
alphaIndexToChar n = chr $ ord 'a' + n


alphaShift :: Int -> Char -> Char
alphaShift shift c = alphaIndexToChar $ (charToAlphaIndex c + shift) `mod` 26

applyToAlphaOnly :: (Char -> a) -> (Char -> a) -> Char -> a
applyToAlphaOnly  alphaFunc nonAlphaFunc c
  | ord c >= ord 'a' && ord c <= ord 'z' = alphaFunc c
  | otherwise = nonAlphaFunc c

vignereCipher :: String -> String -> String
vignereCipher keyword msg = (mkVignereCipher alphaShift) keyword msg

mkVignereCipher :: (Int ->  Char -> Char) -> (String -> String -> String)
mkVignereCipher shifter = \keyword msg ->
  let
        shifts = map charToAlphaIndex keyShifts
        keyShifts = take (length msg) $ cycle keyword
  in
    map (\(shift, c) -> applyToAlphaOnly (shifter shift) id c) $ zip shifts msg

neutraliseVignere :: String -> String -> String
neutraliseVignere keyword encryptedMsg = (mkVignereCipher shifter) keyword encryptedMsg
  where
     shifter shift c = alphaShift (- shift) c
