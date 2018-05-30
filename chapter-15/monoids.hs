module Main where

import Data.Semigroup 
import Data.Monoid hiding ((<>))
import Test.QuickCheck hiding (Success, Failure)
import Test.Hspec

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


prop_semigroupAssociativity :: (Semigroup a, Eq a) => a -> a -> a -> Bool
prop_semigroupAssociativity a b c = (a <> (b <> c)) == ((a <> b) <> c)

prop_monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
prop_monoidLeftIdentity a = (mempty <> a) == a

prop_monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
prop_monoidRightIdentity a = (a <> mempty) == a


main :: IO ()
main = hspec $ do
    describe "Monoids" $ do
        describe "Trivial" $ do
            it "is associative" $ property (prop_semigroupAssociativity :: Trivial -> Trivial -> Trivial -> Bool)
            it "has a left identity element" $ property (prop_monoidLeftIdentity :: Trivial -> Bool)
            it "has a right identity element" $ property (prop_monoidRightIdentity :: Trivial -> Bool)

