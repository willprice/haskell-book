module Validation where
import Data.Monoid
import Control.Monad

import Test.QuickCheck (Arbitrary, arbitrary, oneof)
import Test.QuickCheck.Checkers (quickBatch, EqProp, (=-=), eq)
import Test.QuickCheck.Classes (applicative, functor)


data Validation e a = Failure e
      | Success a
      deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success $ f a


instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (Success f) <*> (Success a) = Success $ f a
  (Success _) <*> (Failure e) = Failure e
  (Failure e) <*> (Success _) = Failure e
  (Failure e1) <*> (Failure e2) = Failure $ e1 `mappend` e2

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = oneof [ liftM Failure arbitrary
                    , liftM Success arbitrary
                    ]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
    quickBatch $ functor val
    quickBatch $ applicative val
    where
        val = Failure 1 :: Validation (Sum Int) (Sum Int, Sum Int, Sum Int)
