import Data.List

data Tree a = Nil
            | Node a (Tree a) (Tree a)

newtype Set a = Set ( Tree a )

instance Eq a => Eq (Tree a) where
    Nil == Nil = True
    Nil == _     = False
    _ == Nil     = False
--    Node x xl xr == Node y yl yr = prepSet ( treeToList ( Node x xl xr ) ) == prepSet ( treeToList ( Node y yl yr ) )

instance Eq a => Eq ( Set a ) where
    (==) = eqSet

-- Removes the duplicate elements 
prepSet :: Ord a => [ a ] -> [ a ]
prepSet x = map head . group . sort $ x

-- Convertes a Tree in to a list by in order algorithm
treeToList :: Eq a => Tree a -> [ a ]
treeToList ( Nil ) = [  ]
treeToList ( Node x Nil Nil ) = [ x ]
treeToList ( Node x ( Node y Nil Nil ) xr ) = y : x : treeToList xr
treeToList ( Node x xl xr ) = treeToList xl ++ [ x ] ++ treeToList xr

-- Returns if a given set is equal to another set
eqSet :: Eq a => Set a -> Set a -> Bool
eqSet ( Set xs ) ( Set ys ) = xs == ys

ex1 = ( Node 1 ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) ( Node 7 Nil Nil ) ) )

