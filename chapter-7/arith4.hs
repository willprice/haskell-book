module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

main = do
    print (roundTrip 4 :: (Num a => a))
    print $ id 4
