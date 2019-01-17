import Control.Applicative
import Data.Monoid


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)


instance Functor (Constant a) where
    -- We have to destructure the value argument to fmap as the type is
    -- Constant a b and the function maps b -> c so we need to construct
    -- a new value of type Constant a c, but since b or c is not present
    -- at the value level we have to do this through reconstructing the
    -- type
    fmap f (Constant a) = (Constant a)


instance Monoid a => Applicative (Constant a) where
    pure = Constant . mempty
    (Constant a) <*> (Constant a') = Constant $ a <> a'


f = Constant $ Sum 1
g = Constant $ Sum 2
