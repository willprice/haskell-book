module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forest of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
            \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines "" = []
myLines lines = case line of
                  "" -> restLines
                  l -> l : restLines
  where line = takeWhile (/= '\n') lines
        restLines = myLines $ drop (1 + length line) lines

shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forest of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)
