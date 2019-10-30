import Control.Monad
import Data.List
import Test.QuickCheck

data Tree a = Nil
            | Node a ( Tree a ) ( Tree a )
              deriving ( Show )

instance Arbitrary a => Arbitrary ( Tree a ) where
    arbitrary = sized gerTree

gerTree :: Arbitrary a => Int -> Gen ( Tree a )
gerTree size
  | size > 0 = do
      val <- arbitrary
      subtree1 <- gerTree ( size `div` 2 )
      subtree2 <- gerTree ( size `div` 2 )
      return ( Node val ( subtree1 ) ( subtree2 ) )
  | otherwise = return ( Nil )

nodesCount :: Tree a -> Int
nodesCount ( Nil ) = 1
nodesCount ( Node v lt rt ) = 1 + ( nodesCount lt ) + ( nodesCount rt )

leafsCount :: Tree a -> Int
leafsCount ( Nil ) = 0
leafsCount ( Node v Nil Nil ) = 1 
leafsCount ( Node v lt rt ) = ( leafsCount lt ) + ( leafsCount rt )

depthTree :: Tree a -> Int
depthTree (Nil) = 0
depthTree (Node x leftNode rightNode) = theDepth
      where theDepth = maximum [ lDepth, rDepth ]
            lDepth = 1 + depthTree leftNode
            rDepth = 1 + depthTree rightNode

isEmpty :: Tree a -> Bool
isEmpty ( Nil ) = True
isEmpty _ = False 

prop1 :: Tree a -> Property
prop1 t = not ( isEmpty t ) ==> ( leafsCount t ) <= ( nodesCount t ) + 1 

prop2 :: Tree a -> Property
prop2 t = not ( isEmpty t ) ==> ( nodesCount t ) <= ( 2^( ( depthTree t ) + 1 ) - 1 )

prop3 :: Tree a -> Property
prop3 t = not ( isEmpty t ) ==> ( nodesCount t ) >= ( ( 2 * h )  + 1 )
    where h | depthTree t == 1 = 0
            | otherwise = depthTree t

