module Data.Optional where

data Optional a = Empty
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Empty
    (Only a) `mappend` (Only b) = Only (a `mappend` b)
    (Only a) `mappend` Empty = Only a
    Empty `mappend` (Only b) = Only b
    Empty `mappend` Empty = Empty

