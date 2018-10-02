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


newtype Identity a = Identity a deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity $ a <> b

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        x <- arbitrary
        return $ Identity x

data Two a b = Two a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    (BoolConj False) <> (BoolConj _) = BoolConj False
    (BoolConj _) <> (BoolConj False) = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary


newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    (BoolDisj True) <> (BoolDisj _) = BoolDisj True
    (BoolDisj _) <> (BoolDisj True) = BoolDisj True

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary


newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    f1 <> f2 = Combine $ (unCombine f1) <> (unCombine f2)

instance (Monoid a, Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ \_ -> mempty
    mappend = (<>)

newtype Comp a = Comp (a -> a)

instance Semigroup (Comp a) where
    (Comp f1) <> (Comp f2) = Comp $ f1 . f2

instance Monoid (Comp a) where
    mempty = Comp id
    mappend = (<>)


newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
    mem1 <> mem2 =
        let
            fn s =
                let
                    (a1, s1) = (runMem mem1) s
                    (a2, s2) = (runMem mem2) s1
                in
                    (a1 <> a2, s2)
        in
            Mem $ fn

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend = (<>)


memTests = describe "Mem" $ do
    it "rmleft == (\"hi\", 1)" $ rmleft `shouldBe` ("hi", Sum 1)
    it "rmright == (\"hi\", 1)" $ rmright `shouldBe` ("hi", Sum 1)
    it "(rmzero :: (String, int)) ==  (\"\", 0)" $ rmzero `shouldBe` ("", Sum 0)
    it "rmleft == runMem f' 0" $ (rmleft == ((runMem f') $ Sum 0)) `shouldBe` True
    it "rmright == runMem f' 0" $ (rmright == ((runMem f') $ Sum 0)) `shouldBe` True
    where
        rmzero = runMem mempty (Sum 0)
        rmleft = runMem (f' <> mempty) (Sum 0)
        rmright = runMem (mempty <> f') (Sum 0)
        f' = Mem $ \s -> ("hi", s <> (Sum 1))


prop_semigroupAssociativity :: (Semigroup a, Eq a) => a -> a -> a -> Bool
prop_semigroupAssociativity a b c = (a <> (b <> c)) == ((a <> b) <> c)

prop_monoidLeftIdentity :: (Monoid a, Eq a) => a -> Bool
prop_monoidLeftIdentity a = (mempty <> a) == a

prop_monoidRightIdentity :: (Monoid a, Eq a) => a -> Bool
prop_monoidRightIdentity a = (a <> mempty) == a

--testMonoidProperties :: (Monoid a, Semigroup a) => String -> (SpecWith (Test.Hspec.Arg Property))
--testMonoidProperties name = describe name $ do
--    it "is associative" $ property (prop_semigroupAssociativity :: a -> a -> a -> Bool)
--    it "has a left identity element" $ (property prop_monoidLeftIdentity :: a -> Bool)
--    it "has a right identity element" $ (property prop_monoidRightIdentity :: a -> Bool)


main :: IO ()
main = hspec $ do
    describe "Monoids" $ do
        describe "Trivial" $ do
            it "is associative" $ property (prop_semigroupAssociativity :: Trivial -> Trivial -> Trivial -> Bool)
            it "has a left identity element" $ property (prop_monoidLeftIdentity :: Trivial -> Bool)
            it "has a right identity element" $ property (prop_monoidRightIdentity :: Trivial -> Bool)
        describe "Identity" $ do
            it "is associative" $ property (prop_semigroupAssociativity :: Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool)
            it "has a left identity element" $ property (prop_monoidLeftIdentity :: Identity (Sum Int) -> Bool)
            it "has a right identity element" $ property (prop_monoidRightIdentity :: Identity (Sum Int) -> Bool)
        describe "BoolConj" $ do
            it "is associative" $ property (prop_semigroupAssociativity :: BoolConj -> BoolConj -> BoolConj -> Bool)
            it "has a left identity element" $ property (prop_monoidLeftIdentity :: BoolConj -> Bool)
            it "has a right identity element" $ property (prop_monoidRightIdentity :: BoolConj -> Bool)
        describe "BoolDisj" $ do
            it "is associative" $ property (prop_semigroupAssociativity :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
            it "has a left identity element" $ property (prop_monoidLeftIdentity :: BoolDisj -> Bool)
            it "has a right identity element" $ property (prop_monoidRightIdentity :: BoolDisj -> Bool)
    memTests
