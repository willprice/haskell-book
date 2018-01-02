import Data.Char


caesar :: Int -> [Char] -> [Char]
caesar shift msg = map (caesarChar shift) msg

caesarChar :: Int -> Char -> Char
caesarChar shift c = if isAlpha c then go shift c else c
  where go shift c = chr $ (((ord c) - (ord 'a') + shift) `mod` 26) + ord 'a'

unCaesar shift encryptedMsg = caesar (-shift) encryptedMsg
