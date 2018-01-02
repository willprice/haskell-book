myWords :: String -> [String]
myWords "" = []
myWords sentence = case word of
                     "" -> restWords
                     w -> w : restWords
  where word = takeWhile (/= ' ') sentence
        restWords = myWords (drop (1 + length word) sentence)
