import Control.Applicative

xs = [1, 2, 3]
xs' = [9, 9, 9]

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)


instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x

mkId = Identity
