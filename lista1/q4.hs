import Data.List

data Tree a = Nil
            | Node a (Tree a) (Tree a)
               

newtype Set a = Set ( Tree a )

empty :: Set a
empty = Set Nil

sing :: a -> Set a
sing x = Set ( Node x Nil Nil)

instance Ord a => Eq ( Set a ) where
    (==) = eqSet


instance Ord a => Eq (Tree a) where
    Nil == Nil = True
    Nil == _     = False
    _ == Nil     = False
    Node x xl xr == Node y yl yr = prepSet ( treeToList ( Node x xl xr ) ) == prepSet ( treeToList ( Node y yl yr ) )

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
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet ( Set xs ) ( Set ys ) = xs == ys

ex2 = ( Node 1 ( Node 2 ( Node 4 ( Node 1 Nil Nil ) Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) ( Node 7 Nil Nil ) ) )

ex1 = ( Node 1 ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) ( Node 7 Nil Nil ) ) )

ex3 = ( Node 1 ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) Nil ) )


memSet :: Ord a => Set a -> a -> Bool -- Verifica se o elemento faz parte da arvore
memSet ( Set ( Nil ) ) _ = False
memSet ( Set ( Node x xl xr ) ) y
    | x == y = True
    | x < y = memSet ( Set xr ) y
    | otherwise = memSet ( Set xl ) y


union :: Ord a => Set a -> Set a -> Set a -- Faz a uniao de duas arvores
union ( Set xs ) ( Set ys ) = Set ( uni xs ys )

insert :: Ord a => Tree a -> [ a ] -> Tree a
insert t [] = t 
insert t [] = Nil
uni 

--rootVal :: Ord a => Tree a -> a
--rootVal 

--{-
---} 
-- inter :: Ord a => Set a -> Set a -> Set a
-- diff :: Ord a => Set a -> Set a -> Set a
-- eqSet :: Eqa =>Seta->Seta->Bool
-- subSet :: Ord a => Set a -> Set a -> Bool
-- makeSet :: Ord a => [a] -> Set a
-- mapSet :: Ord b => (a -> b) -> Set a -> Set b
-- filterSet :: (a -> Bool) -> Set a -> Set a
-- foldSet :: (a -> a -> a) -> a -> Set a -> a
-- showSet :: (a -> String ) -> Set a -> String
-- card :: Set a -> Int
-- flatten :: Set a -> [a]
   

