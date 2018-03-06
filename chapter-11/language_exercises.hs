import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

splitOn :: Char -> String -> [String]
splitOn c str = case dropWhile ((==) c) str of
  "" -> []
  str' -> part : splitOn c str''
    where  (part, str'') = break ((==) c) str'


capitalizeParagraph :: String -> String
capitalizeParagraph para = concat $ (intersperse ". "
                         $ map (capitalizeWord . trimWhitespace) sentences) ++ ["."]
  where sentences = splitOn '.' para


trimWhitespace :: String -> String
trimWhitespace = dropWhile isSpace . dropWhileEnd isSpace


-- Yuck
-- Try again after looking at https://github.com/dwayne/haskell-programming/blob/master/ch11/Misc.hs

capitalizeParagraph' :: String -> String
capitalizeParagraph' para = go True para
  where
    go _ [] = []
    go capitalizeNextChar (' ':cs) = ' ' : go capitalizeNextChar cs
    go _ ('.':cs) = '.' : go True cs
    go True (c:cs) = toUpper c : go False cs
    go False (c:cs) = c : go False cs


data PadDigit = PadDigit { padDigit :: Char
                         , padChars :: [Char]
                         }

data PhonePad = PhonePad [PadDigit]

convo :: [String]
convo = [ "Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have u ever tasted alcohol"
        , "Lol ya"
        , "Wow ur cool haha. Ur turn"
        , "Ok. Do u think I am pretty Lol"
        , "Lol ya"
        , "Just making sure rofl ur turn"
        ]

type Digit = Char
type Presses = Int


phonePad = PhonePad [ (PadDigit '1' [])
                    , (PadDigit '2' "abc")
                    , (PadDigit '3' "def")
                    , (PadDigit '4' "ghi")
                    , (PadDigit '5' "jkl")
                    , (PadDigit '6' "mno")
                    , (PadDigit '7' "pqrs")
                    , (PadDigit '8' "tuv")
                    , (PadDigit '9' "wxyz")
                    , (PadDigit '*' "^")
                    , (PadDigit '0' "+ ")
                    , (PadDigit '#' ".,")
                    ]

padForDigit:: PhonePad -> Char -> Maybe PadDigit
padForDigit (PhonePad []) _ = Nothing
padForDigit pad@(PhonePad (digit:digits)) c
  | c `elem` padChars digit = Just digit
  | otherwise = padForDigit (PhonePad digits) c


reverseTaps :: PhonePad -> Char -> Maybe [(Digit, Presses)]
reverseTaps phone c
  | isUpper c = ((:) ('*', 1 :: Int)) <$> reverseTaps phone (toLower c)
  | otherwise = do
      pad <- padForDigit phone c
      presses <- ((+) 1) <$> (c `elemIndex` (padChars pad))
      digit <- return $ padDigit pad
      return [(digit, presses)]

taps :: PhonePad -> String -> Maybe [(Digit, Presses)]
taps phone s = concat <$> traverse (reverseTaps phone) s


-- Hutton's Razor

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = (eval a) + (eval b)


printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = (printExpr a) ++ " + " ++ (printExpr b)
