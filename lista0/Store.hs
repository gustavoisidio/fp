module Store
   ( Store,
     initial,     -- Store
     value,       -- Store -> Var -> Integer
     update,       -- Store -> Var -> Integer -> Store
     sort         -- Store -> Store
    ) where

-- Var is the type of variables.

type Var = Char

-- The implementation is given by a newtype declaration, with one
-- constructor, taking an argument of type [ (Integer,Var) ].

data Store = Store [ (Integer,Var) ]

instance Eq Store where
  (Store sto1) == (Store sto2) = (sto1 == sto2)

instance Show Store where
  showsPrec n (Store sto) = showsPrec n sto
--
initial :: Store
initial = Store []

value  :: Store -> Var -> Maybe Integer
value (Store []) v         = Nothing
value (Store ((n,w):sto)) v
  | v==w            = Just n
  | otherwise       = value (Store sto) v

update  :: Store -> Var -> Integer -> Store
update (Store sto) v n = Store ((n,v):sto)

sort :: Store -> Store
sort ( Store sto ) = Store $ qSortTuple sto

qSortTuple :: [(Integer, Var)] -> [(Integer, Var)]
qSortTuple [] = []
qSortTuple ((a, b):xs) = qSortTuple esquerda ++ [(a, b)] ++ qSortTuple direita
    where esquerda = [(a, y) | (a, y) <- xs, ( fromEnum y ) < ( fromEnum b )]
          direita = [(a, y) | (a, y) <- xs, ( fromEnum y ) >= ( fromEnum b )]

ex1 = Store [ ( 1, 'b' ), ( 4, 'i' ), ( 3, 's' ), ( 23, 'r' ), ( 48, 'h' ), ( 11, 'z' ), (  6, 'e' ) ]
