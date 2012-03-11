module ZipList where

import System.Random

-- single linked list
data List a = Nil | Node a (List a) deriving (Show)

-- A location has a pointer to prev,
-- and a pointer to next. So the zipper
-- is made out of two singly linked lists
-- pointing away from current location.
data Loc a = Loc (List a) (List a) deriving (Show)


moveLeft :: Loc a -> Loc a
moveLeft (Loc Nil _) = error "No previous node"
moveLeft (Loc (Node v p) n) = Loc p (Node v n)

moveRight :: Loc a -> Loc a
moveRight (Loc _ Nil) = error "No next node"
moveRight (Loc p (Node v n)) = Loc (Node v p) n

randomInts :: Int -> IO [Int]
randomInts n = do
                   g <- newStdGen
                   return $ take n $ randomRs (0,1000) g

insert :: List a -> a -> List a
insert = flip Node

randomList :: IO (List Int)
randomList = do
                 is <- randomInts 10
                 return $ foldl insert Nil is
