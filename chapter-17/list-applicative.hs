import Data.Monoid hiding ((<>))
import Control.Applicative
import Data.Semigroup


data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

instance Semigroup (List a) where
    as <> Nil = as
    Nil <> as = as
    (Cons a a_tail) <> bs = Cons a (a_tail <> bs)


instance Monoid (List a) where
    mempty = Nil
    mappend = (<>)


instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a tail) = Cons (f a) $ fmap f tail


instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f f_tail) <*> as = (fmap f as) `mappend` (f_tail <*> as)
