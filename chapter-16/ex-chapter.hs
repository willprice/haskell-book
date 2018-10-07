{-# LANGUAGE FlexibleInstances #-}
import GHC.Arr
-- Can we write a valid Functor instance for the following datatypes?

-- 1.  `data Bool = False | True` - no, kind is *

-- 2.
data BoolAndSomethingElse a = False' a | True' a

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' $ f a
    fmap f (True' a) = True' $ f a

-- 3.
data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
    fmap f Falsish = Falsish
    fmap f (Truish a) = Truish $ f a


-- 4.
newtype Mu f = Inf { outF :: f (Mu f) }
-- Mu :: (* -> *) -> *
-- Because we can't destructure the types to get kind * -> * we can't
-- declare an instance of Functor (I think...)


-- 5.

data D = D (Array Word Word) Int Int
-- D :: *, therefore we can't define a functor instance

-------------------------------------------------------------------------------

-- Rearrange the arguments to the type constructor of the datatype so
-- the Functor instance works

-- 1.
data Sum b a = First a | Second b
              deriving (Show, Eq)

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b


-- 2.
data Company a c b = DeepBlue a c 
                   | Something b
              deriving (Show, Eq)

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More b a = L a b a 
              | R b a b
              deriving (Show, Eq)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

-------------------------------------------------------------------------------

-- Write Functor instances for the following datatypes

-- 1.
data Quant a b = Finance 
               | Desk a 
               | Bloor b

instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor $ f b

-- 2.
newtype K a b = K a
    deriving (Eq, Show)

instance Functor (K a) where
    fmap f (K a) = K a

-- 3.
newtype Flip f a b = Flip (f b a)
    deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap f (Flip (K a)) = Flip $ K $ f a

-- 4.
data EvilGoateeConst a b = GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

-- 5.
data LiftItOut f a = LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9.
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a cdr) = Cons (f a) (fmap f cdr)

-- 10.
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving (Eq, Show)

instance Functor GoatLord where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats fa fa' fa'') = MoreGoats (fmap f fa) (fmap f fa') (fmap f fa'')

-- 11.

data TalkToMe a = Halt 
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
    fmap f Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read fs) = Read $ f . fs
