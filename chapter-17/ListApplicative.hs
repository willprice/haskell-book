module ListApplicative where

import Data.Monoid hiding ((<>))
import Control.Applicative
import Control.Monad
import Data.Semigroup

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


toMyList [] = Nil
toMyList (x:xs) = Cons x $ toMyList xs


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
    (Cons f f_tail) <*> as = (fmap f as) <> (f_tail <*> as)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = sized list'
        where list' 0 = return Nil
              list' n | n > 0 = oneof [return Nil, liftM2 Cons arbitrary tail]
                  where tail = list' (n - 1)

instance Eq a => EqProp (List a) where
    (=-=) = eq


newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)


instance (Semigroup a) => Semigroup (ZipList' a) where
    (ZipList' Nil) <> b = b
    a <> (ZipList' Nil) = a
    (ZipList' (Cons a tail_a)) <> (ZipList' (Cons b tail_b)) =
        ZipList' $ Cons (a <> b) tail
            where (ZipList' tail) = (ZipList' tail_a) <> (ZipList' tail_b)


instance (Monoid a) => Monoid (ZipList' a) where
    mempty = ZipList' Nil
    mappend = (<>)

instance Functor ZipList' where
    fmap f (ZipList' l) = ZipList' $ fmap f l

instance Applicative ZipList' where
    pure a = ZipList' $ Cons a Nil
    (ZipList' Nil) <*> _ = ZipList' Nil
    _ <*> (ZipList' Nil) = ZipList' Nil
    (ZipList' (Cons f fs)) <*> (ZipList' (Cons x xs)) = ZipList' $ Cons (f x) tail
        where ZipList' tail = (ZipList' fs) <*> (ZipList' xs)


instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
    (=-=) = eq

main :: IO ()
main = do
    quickBatch $ semigroup l
    quickBatch $ monoid l
    quickBatch $ functor l
    quickBatch $ applicative l

    quickBatch $ semigroup zl
    quickBatch $ monoid zl
    quickBatch $ functor zl
    where
        l = Cons (1 :: Int, 1 :: Int, 1 :: Int) Nil
        zl = ZipList' $ Cons (1 :: Sum Int, 1 :: Sum Int, 1 :: Sum Int) Nil
