import BadMonoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo) ]


instance EqProp Bull where (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)
