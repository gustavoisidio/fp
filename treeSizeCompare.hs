data Tree a = Nil
            | Node a (Tree a) (Tree a)

instance Eq a => Eq (Tree a) where
    Nil == Nil = True
    Nil == _     = False
    _ == Nil     = False
    Node x xl xr == Node y yl yr = (x == y) && xl == yl && xr == yr

instance Ord a => Ord ( Tree a ) where
    Nil < Nil = False
    Node x xl xr < Node y yl yr = ( depthTree $ Node x xl xr ) < ( depthTree $ Node y yl yr )
    Nil > Nil = False
    Node x xl xr > Node y yl yr = ( depthTree $ Node x xl xr ) > ( depthTree $ Node y yl yr )
    Nil <= Nil = True
    Node x xl xr <= Node y yl yr = ( ( depthTree $ Node x xl xr ) < ( depthTree $ Node y yl yr ) ) || ( Node x xl xr == Node y yl yr )
    Nil >= Nil = True
    Node x xl xr >= Node y yl yr = ( ( depthTree $ Node x xl xr ) > ( depthTree $ Node y yl yr ) ) || ( Node x xl xr == Node y yl yr )


depthTree :: Tree a -> Int
depthTree (Nil) = 0
depthTree (Node x leftNode rightNode) = theDepth
      where theDepth = maximum [ lDepth, rDepth ]
            lDepth = 1 + depthTree leftNode
            rDepth = 1 + depthTree rightNode

data WitchOne = First | Second | Whatever deriving ( Show )

biggerTree :: Tree a -> Tree a -> WitchOne
biggerTree tree1 tree2
    | depthTree tree1 == depthTree tree2 = Whatever
    | depthTree tree1 < depthTree tree2 = Second
    | otherwise = First 

ex1 = ( Node 2 ( Node 4 ( Node 6 Nil Nil ) Nil ) ( Node 2 ( Node 4 ( Node 6 ( Node 7 Nil Nil ) Nil ) Nil ) Nil ) )
ex2 = ( Node 2 ( Node 4 ( Node 6 ( Node 7 Nil Nil ) Nil ) Nil ) Nil )


data TreeI tipo = NilI
                | NodeI tipo ( TreeI tipo ) ( TreeI tipo )


