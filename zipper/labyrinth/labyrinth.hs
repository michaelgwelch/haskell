module Labyrinth where

data Node a = DeadEnd a
            | Passage a (Node a)
            | Fork    a (Node a) (Node a) deriving (Show, Eq)

-- Exercise 1 implement get and put
get :: Node a -> a
get (DeadEnd v) = v
get (Passage v _) = v
get (Fork v _ _) = v

put :: a -> Node a -> Node a
put v (DeadEnd _) = DeadEnd v
put v (Passage _ n) = Passage v n
put v (Fork _ n1 n2) = Fork v n1 n2

-- Exercise 2 create a concrete labyrinth form image
-- Assume that the center is (0,0)

path :: Node (Int, Int)
path = Fork (0,2) 
       (
           Fork (-2,0) (DeadEnd (0,-2)) (DeadEnd (-1,0))
       )
       (
           Passage (2,0)
           (
               Fork (1,0) (Passage (0,1) (DeadEnd (0,0))) (DeadEnd (0,-1))
           )          
       )


data Branch = KeepStraightOn
            | TurnLeft
            | TurnRight

type Thread = [Branch]

turnRight :: Thread -> Thread
turnRight t = t ++ [TurnRight]

retrieve :: Thread -> Node a -> a
retrieve []                  n             = get n
retrieve (KeepStraightOn:bs) (Passage _ n) = retrieve bs n
retrieve (TurnLeft:bs)       (Fork _ l _)  = retrieve bs l
retrieve (TurnRight:bs)      (Fork _ _ r)  = retrieve bs r

update :: (a -> a) -> Thread -> Node a -> Node a
