{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (n, str) = n > 42 && length str > 40

instance TooMany (Int, Int) where
    tooMany (n1, n2) = tooMany (n1 + n2)

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (a, b) = tooMany b || tooMany a
