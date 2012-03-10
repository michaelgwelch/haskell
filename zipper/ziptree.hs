
import Prelude hiding (Right, Left)
import System.Random

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)

data Loc a = Loc (Tree a) (Context a) deriving (Show, Eq)

data Context a = Top
               | Left a (Tree a) (Context a)
               | Right a (Tree a) (Context a) deriving (Show, Eq)

data Ord a => Zipper a = Zip (Loc a) deriving (Show, Eq)

zipper :: Ord a => Zipper a
zipper = Zip (Loc Nil Top)

moveUp :: Loc a -> Loc a
moveUp (Loc _ Top) = error "Already at top of tree"
moveUp (Loc lt (Right e rt p)) = Loc (Node e lt rt) p
moveUp (Loc rt (Left e lt p)) = Loc (Node e lt rt) p

moveLeft :: Loc a -> Loc a
moveLeft (Loc Nil _) = error "No node to the left"
moveLeft (Loc (Node e lt rt) c) = Loc lt (Right e rt c)

moveRight :: Loc a -> Loc a
moveRight (Loc Nil _) = error "No node to the right"
moveRight (Loc (Node e lt rt) c) = Loc rt (Left e lt c)


-- Adds a value of a to the tree given
-- a location. It assumes that the given node
-- is the root
add :: Ord a => Tree a -> a -> Tree a
add Nil e = Node e Nil Nil
add (Node e' lt rt) e | e < e' = Node e' (add lt e) rt
                      | e > e' = Node e' lt (add rt e)
                      | otherwise = Node e' lt rt


count :: Tree a -> Integer
count Nil = 0
count (Node _ lt rt) = 1 + count lt + count rt

height :: Tree a -> Integer
height Nil = 0;
height (Node _ lt rt) = 1 + max (height lt) (height rt)


randomInts :: Int -> IO [Int]
randomInts n = do
                 g <- newStdGen
                 return $ take n $ randomRs (0,1000) g

randomTree :: IO (Tree Int)
randomTree = do 
                 is <- randomInts 100
                 return $ foldl (\t -> \v -> add t v) Nil is 
