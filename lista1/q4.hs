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

ex4 = ( Node 2 ( Node 4 Nil Nil ) ( Node 5 Nil Nil ) )

memSet :: Ord a => Set a -> a -> Bool -- Verifica se o elemento faz parte da arvore
memSet ( Set ( Nil ) ) _ = False
memSet ( Set ( Node x xl xr ) ) y
    | x == y = True
    | x < y = memSet ( Set xr ) y
    | otherwise = memSet ( Set xl ) y

union :: Ord a => Set a -> Set a -> Set a -- Faz a uniao de duas arvores
union ( Set xs ) ( Set ys ) = Set ( insertTree xs ( treeToList ys ) )

insertTree :: Ord a => Tree a -> [ a ] -> Tree a
insertTree t [] = t 
insertTree ( Nil ) ( y:ys ) = insertTree ( Node y Nil Nil ) ys
insertTree ( Node x xr xl ) ( y:ys )
    | x == y = insertTree ( Node x xr xl ) ys
    | x < y = insertTree ( Node x xl ( insertTree xr [ y ] ) ) ys
    | otherwise = insertTree ( Node x ( insertTree xl [ y ] ) xr ) ys

inter :: Ord a => Set a -> Set a -> Set a
inter ( Set s1 ) ( Set s2 ) = Set $ insertTree ( Nil ) [head x | x <- groupByDuplicates, length x > 1] 
    where treeTogether = ( treeToList s1 ) ++ ( treeToList s2 )
          groupByDuplicates = group . sort $ treeTogether

makeSet :: Ord a => Tree a -> Set a
makeSet t = Set ( insertTree ( Nil ) $ map head . group . sort $ treeToList t ) 

card :: Eq a => Set a -> Int
card ( Set t ) = length $ treeToList t

diff :: Ord a => Set a -> Set a -> Set a
diff ( Set s1 ) ( Set s2 ) = Set $ insertTree ( Nil ) [head x | x <- groupByDuplicates, length x == 1]
    where treeTogether = ( treeToList s1 ) ++ ( treeToList s2 )
          groupByDuplicates = group . sort $ treeTogether

subSet :: Ord a => Set a -> Set a -> Bool
subSet ( Set s1 ) ( Set s2 ) = subS ( treeToList s1 ) ( treeToList s2 )

subS :: Eq a => [ a ] -> [ a ] -> Bool
subS [  ] _ = True 
subS ( x:xs ) ys = not ( elem x ys ) && subS xs ys 

mapSet :: ( Ord b, Eq a ) => (a -> b) -> Set a -> Set b
mapSet f ( Set t ) = Set $ insertTree ( Nil ) ( map f $ treeToList t ) 

flatten :: Eq a => Set a -> [a]
flatten ( Set t ) = treeToList t

filterSet :: ( Eq a, Ord a ) => (a -> Bool) -> Set a -> Set a
filterSet f ( Set t ) = Set $ insertTree ( Nil ) ( filter f $ treeToList t )

foldSet :: Eq a => (a -> a -> a) -> a -> Set a -> a
foldSet f x ( Set t ) = foldl f x $ treeToList t 

--showSet :: (a -> String ) -> Set a -> String
   

