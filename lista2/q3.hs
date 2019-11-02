import Set
import Relation 
import Data.List hiding ( union )
import Test.QuickCheck

gr1 = makeSet ([(1, 2), (1, 3), (3, 2), (3, 4), (4, 2), (2, 4)]) :: ( Relation Int ) 


instance ( Arbitrary a, Ord a) => Arbitrary ( Set a ) where
    arbitrary = sized gerSet

instance Show a => Show ( Set a ) where
    show = show . flatten

gerSet :: ( Arbitrary a, Ord a ) => Int -> Gen ( Set a )
gerSet size
    | size > 0 = do
        n <- arbitrary
        setTail <- gerSet ( size `div` 2 )
        return ( union ( sing n ) setTail )
    | otherwise = return empty

prop1 :: Ord a => Relation a -> Property
prop1 t
  = ( not $ null $ flatten t )
  ==> ( sort $ depthFirst t v ) == ( sort $ depthFirst t v ) 
    where v = (fst.head.flatten $ t)

prop2 :: ( Ord a ) => Relation a -> Property
prop2 t 
  = ( not $ null $ flatten t )
  ==> ( head $ depthFirst t v ) == ( head $ depthFirst t v )
    where v = (fst.head.flatten $ t)


