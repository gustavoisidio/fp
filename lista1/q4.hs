import Data.List

data Tree a = Nil
            | Node a (Tree a) (Tree a)
               

newtype Set a = Set ( Tree a )

empty :: Set a
empty = Set Nil

sing :: a -> Set a
sing x = Set ( Node x Nil Nil)

instance Eq a => Eq ( Set a ) where
    (==) = eqSet

class Eq a => Ord a where
    eqSet :: Set a -> Set a -> Bool
    eqSet ( Set xs ) ( Set ys ) = prepSet ( treeToList xs ) == prepSet ( treeToList ys )
    
    prepSet :: [ a ] -> [ a ]
    prepSet x = map head . group . sort $ x

--instance Eq a => Eq (Tree a) where
  --  Nil == Nil = True
   -- Nil == _     = False
   -- _ == Nil     = False
   -- a1 == a2 = eqSet a1 a2 -- Incorreto

--prepSet :: ( Ord a, Num a ) => [ a ] -> [ a ]
--prepSet x = map head . group . sort $ x

treeToList :: Eq a => Tree a -> [ a ]
treeToList ( Nil ) = [  ]
treeToList ( Node x Nil Nil ) = [ x ]
treeToList ( Node x ( Node y Nil Nil ) xr ) = y : x : treeToList xr
treeToList ( Node x xl xr ) = treeToList xl ++ [ x ] ++ treeToList xr

--eqSet :: (Ord a, Num a) => Set a -> Set a -> Bool
--eqSet ( Set xs ) ( Set ys ) = prepSet ( treeToList xs ) == prepSet ( treeToList ys ) 



ex1 = ( Node 1 ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) ) ( Node 3 ( Node 6 Nil Nil ) ( Node 7 Nil Nil ) ) )


-- empty :: Set a
-- sing :: a -> Set a
-- memSet :: Ord a => Set a -> a -> Bool -- Verifica se o elemento faz parte da arvore
-- union :: Ord a => Set a -> Set a -> Set a -- Faz a uniao de duas arvores
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
   

--λ> map head . group . sort $ [1..3]
--[1,2,3]
