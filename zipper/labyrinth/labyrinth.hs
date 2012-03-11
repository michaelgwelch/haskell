module Labyrinth where

data Node a = DeadEnd a
            | Passage a (Node a)
            | Fork    a (Node a) (Node a)

get :: Node a -> a
get (DeadEnd v) = v
get (Passage v _) = v
get (Fork v _ _) = v

put :: a -> Node a -> Node a
put v (DeadEnd _) = DeadEnd v
put v (Passage _ n) = Passage v n
put v (Fork _ n1 n2) = Fork v n1 n2
