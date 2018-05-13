{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Optional
import Data.Monoid

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        elements [Only a, Empty]



prop_OptionalMonoid_RightIdentity e = (e `mappend` mempty) == e
    where types = (e :: Sum Integer)
prop_OptionalMonoid_LeftIdentity e = (mempty `mappend` e) == e
    where types = (e :: Sum Integer)
prop_OptionalMonoid_Associativity e1 e2 e3 = ((e1 `mappend` e2) `mappend` e3) == (e1 `mappend` (e2 `mappend` e3))
    where types = (e1 :: Sum Integer, e2 :: Sum Integer, e3 :: Sum Integer)


return []
checkProperties = $quickCheckAll


main :: IO ()
main = do
    checkProperties
    return ()

