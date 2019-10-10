module Deque
  ( Deque ,
    emptyDQ ,       --  Queue a
    isEmptyDQ ,     --  Queue a -> Bool
    addDQ ,         --  a -> Queue a -> Queue a
    remDQ           --  Queue a -> (  a , Queue a )
   ) where

newtype Deque a = Deque [a]

emptyDQ :: Deque a

emptyDQ = Deque []

isEmptyDQ :: Deque a -> Bool

isEmptyDQ (Deque []) = True
isEmptyDQ _ = False

addDQ :: a -> Deque a -> Deque a
addDQ x (Deque xs) = Deque (xs++[x])

remDQ :: Deque a -> ( a, Deque a )
remDQ q@(Deque xs)
  | not (isEmptyDQ q) = (head xs, Deque (tail xs))
  | otherwise = error "remDQ"
