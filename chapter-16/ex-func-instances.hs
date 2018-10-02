module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec

newtype Identity a = Identity a
    deriving (Eq, Show)

data Pair a = Pair a a
    deriving (Eq, Show)

data Two a b = Two a b
    deriving (Eq, Show)

data Three a b c = Three a b c
    deriving (Eq, Show)

data Three' a b = Three' a b b
    deriving (Eq, Show)

data Four a b c d = Four a b c d
    deriving (Eq, Show)

data Four' a b = Four' a a a b
    deriving (Eq, Show)

-- Can't implement a Functor instance for Trivial since it has kind *
-- not * -> *
data Trivial = Trivial
    deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = arbitrary >>= (return . Identity)


instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        x1 <- arbitrary 
        x2 <- arbitrary
        return $ Pair x1 x2


instance Functor (Two a) where
    fmap f (Two a1 a2) = Two a1 (f a2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x1 <- arbitrary 
        x2 <- arbitrary
        return $ Two x1 x2


instance Functor (Three a1 a2) where
    fmap f (Three a1 a2 a3) = Three a1 a2 (f a3)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x1 <- arbitrary 
        x2 <- arbitrary
        x3 <- arbitrary
        return $ Three x1 x2 x3


instance Functor (Three' a) where
    fmap f (Three' a1 b1 b2) = Three' a1 (f b1) (f b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        x1 <- arbitrary 
        x2 <- arbitrary
        x3 <- arbitrary
        return $ Three' x1 x2 x3


instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d


instance Functor (Four' a) where
    fmap f (Four' a b1 b2 b3) = Four' a (f b1) (f b2) (f b3)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary 
        b1 <- arbitrary
        b2 <- arbitrary
        b3 <- arbitrary
        return $ Four' a b1 b2 b3


prop_functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
prop_functorIdentity f = fmap id f == f

prop_functorCompose :: (Functor f, Eq (f c)) => Fun b c -> Fun a b -> f a -> Bool
prop_functorCompose (Fun _ g) (Fun _ f) x = (fmap g (fmap f x)) == (fmap (g . f) x)

main = hspec $ do
    describe "Functors" $ do
        describe "Identity" $ do
            it "satisfies the identity law" $ property (prop_functorIdentity :: Identity Int -> Bool)
            it "satisfies the composition law" $ property (prop_functorCompose :: Fun Int Int -> Fun Int Int  -> Identity Int -> Bool)
        describe "Pair" $ do
            it "satisfies the identity law" $ property (prop_functorIdentity :: Pair Int -> Bool)
            it "satisfies the composition law" $ property (prop_functorCompose :: Fun Int Int -> Fun Int Int  -> Pair Int -> Bool)
        describe "Two" $ do
            it "satisfies the identity law" $ property (prop_functorIdentity :: Two String Int -> Bool)
            it "satisfies the composition law" $ property (prop_functorCompose :: Fun Int Int -> Fun Int Int  -> Two String Int -> Bool)
        describe "Three" $ do
            it "satisfies the identity law" $ property (prop_functorIdentity :: Three Int String Int -> Bool)
            it "satisfies the composition law" $ property (prop_functorCompose :: Fun Int Int -> Fun Int Int  -> Three Int String Int -> Bool)
        describe "Three'" $ do
            it "satisfies the identity law" $ property (prop_functorIdentity :: Three' String Int -> Bool)
            it "satisfies the composition law" $ property (prop_functorCompose :: Fun Int Int -> Fun Int Int  -> Three' String Int -> Bool)
        describe "Four" $ do
            it "satisfies the identity law" $ property (prop_functorIdentity :: Four Int Int String Int -> Bool)
            it "satisfies the composition law" $ property (prop_functorCompose :: Fun Int Int -> Fun Int Int  -> Four Int Int String Int -> Bool)
        describe "Four'" $ do
            it "satisfies the identity law" $ property (prop_functorIdentity :: Four' String Int -> Bool)
            it "satisfies the composition law" $ property (prop_functorCompose :: Fun Int Int -> Fun Int Int  -> Four' String Int -> Bool)
