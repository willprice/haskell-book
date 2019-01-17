module BadMonoid where

import Data.Monoid


data Bull = Fools | Twoo
    deriving (Eq, Show)


instance Semigroup Bull where
    _ <> _ = Fools


instance Monoid Bull where
    mempty = Fools
    mappend = (<>)
