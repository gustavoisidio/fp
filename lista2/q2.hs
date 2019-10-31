import Data.Set
import Test.QuickCheck

prop1 :: Ord a => Set a -> Set a -> Bool
prop1 a b = ( union a b ) == ( union b a ) 

prop2 :: Ord a => Set a -> Set a -> Bool
prop2 a b = isSubsetOf a ( union a b )

prop3 :: Ord a => Set a -> Bool
prop3 a = ( union a a ) == a 

prop4 :: Ord a => Set a -> Set a -> Bool
prop4 a b = ( intersection a b ) == ( intersection b a )

prop5 :: Ord a => Set a -> Set a -> Set a -> Bool
prop5 a b c = ( intersection a ( intersection b c ) ) == ( intersection ( intersection a b ) c )
