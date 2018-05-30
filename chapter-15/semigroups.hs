module Main where

import Data.Semigroup
import Data.Monoid hiding ((<>))
import Test.QuickCheck hiding (Success, Failure)
import Test.Hspec


data Trivial = Trivial
    deriving (Eq, Show)


instance Semigroup Trivial where
    Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial


newtype Identity a = Identity a
    deriving (Eq, Show)


instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

data Two a b = Two a b
    deriving (Eq, Show)


instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2) 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b


data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c


data Four a b c d = Four a b c d
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d


newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    (BoolConj _) <> (BoolConj _) = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = do
        b <- arbitrary
        return $ BoolConj b


newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> (BoolDisj _) = BoolDisj True
    (BoolDisj _) <> (BoolDisj True) = BoolDisj True
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = do
        b <- arbitrary
        return $ BoolDisj b


data Or a b = Fst a | Snd b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    a <> _ = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        or <- elements [Fst a, Snd b]
        return or


newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    c1 <> c2 = Combine $ (unCombine c1) <> (unCombine c2)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return $ Combine f

-- Without an Eq instance we can't test associativity
-- We'd probably been to do something a bit more clever
-- and actually run the function and check equality of output.


newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    a <> b = Comp $ (unComp a) . (unComp b)


data Validation a b = Failure a
                    | Success b
                    deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
    s@(Success _) <> (Success _) = s
    s@(Success _) <> (Failure _) = s
    (Failure _) <> s@(Success _) = s
    (Failure f1) <> (Failure f2) = Failure (f1 <> f2)

success :: Int -> Validation String Int
success = Success

failure :: String -> Validation String Int
failure = Failure

validationTests = it "Semigroup (Validation String Int)" $ do
    (success 1 <> failure "blah") `shouldBe` (Success 1)
    (failure "woot" <> failure "blah") `shouldBe` (Failure "wootblah")
    (success 1 <> success 2) `shouldBe` (Success 1)
    (failure "woot" <> success 2) `shouldBe` (Success 2)



semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = Identity (Sum Integer) ->
                     Identity (Sum Integer) ->
                     Identity (Sum Integer) -> Bool

type TwoAssoc = Two (Sum Integer) (Sum Integer) ->
                Two (Sum Integer) (Sum Integer) ->
                Two (Sum Integer) (Sum Integer) -> Bool

type ThreeAssoc = Three (Sum Integer) (Sum Integer) (Sum Integer) ->
                  Three (Sum Integer) (Sum Integer) (Sum Integer) ->
                  Three (Sum Integer) (Sum Integer) (Sum Integer) -> Bool

type FourAssoc = Four (Sum Integer) (Sum Integer) (Sum Integer) (Sum Integer) ->
                 Four (Sum Integer) (Sum Integer) (Sum Integer) (Sum Integer) ->
                 Four (Sum Integer) (Sum Integer) (Sum Integer) (Sum Integer) -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc = Or (Sum Integer) (Product Integer) ->
               Or (Sum Integer) (Product Integer) ->
               Or (Sum Integer) (Product Integer) -> Bool

type CombineAssoc = Combine (Sum Integer) (Sum Integer) ->
                    Combine (Sum Integer) (Sum Integer) ->
                    Combine (Sum Integer) (Sum Integer) -> Bool

main :: IO ()
main = hspec $ do
    describe "Semigroup" $ do
        describe "Associativity" $ do
            it "trivial" $ property (semigroupAssoc :: TrivAssoc)
            it "Identity" $ property (semigroupAssoc :: IdentityAssoc)
            it "Two" $ property (semigroupAssoc :: TwoAssoc)
            it "Three" $ property (semigroupAssoc :: ThreeAssoc)
            it "Four" $ property (semigroupAssoc :: FourAssoc)
            it "BoolConj" $ property (semigroupAssoc :: BoolConjAssoc)
            it "BoolDisj" $ property (semigroupAssoc :: BoolDisjAssoc)
            it "Or" $ property (semigroupAssoc :: OrAssoc)
        validationTests
