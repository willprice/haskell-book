mySplit :: String -> Char -> [String]
mySplit "" delim = []
mySplit str delim = case element of
                  "" -> splitChars
                  e -> e : splitChars
  where element = takeWhile (/= delim) str
        splitChars = mySplit (drop (1 + length element) str) delim
