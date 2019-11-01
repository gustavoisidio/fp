import Set
import Relation 
import Data.List hiding ( union )
import Test.QuickCheck
import Data.Graph

-- newtype Set a = Set [a]
-- type Relation a = Set (a,a)
-- breadthFirst :: Ord a => Relation a -> a -> [a]

gr1 = makeSet ([(1, 2), (1, 3), (3, 2), (3, 4), (4, 2), (2, 4)]) :: ( Relation Int ) 


{-
instance Arbitrary a => Arbitrary ( Set a ) where
    arbitrary = makeSet $ sized gerSet

gerSet :: Arbitrary a => Int -> Gen ( [a] )
gerSet size
    | size > 0 = do
        n <- arbitrary
        setTail <- gerSet ( size `div` 2 )
        return ( n : setTail )
    | otherwise = return [  ]

instance Arbitrary a => Arbitrary ( Relation a) where
    arbitrary = makeSet $ sized gerGraph

gerGraph :: Arbitrary a => Int -> Gen ( [( a,a )] )
gerGraph size
  | size > 0 = do 
    n1 <- arbitrary
    n2 <- arbitrary
    graphTail <- gerGraph ( size `div` 2 )
    return ( ( n1,n2 ) : graphTail ) 
  | otherwise = return []


-}

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


