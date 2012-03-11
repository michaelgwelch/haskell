
data Tree a = Leaf a | Branch (Tree a) (Tree a)

data Context a = Top
               | Left (Tree a) (Context a)
               | Right (Tree a) (Context a)

data Loc = Loc (Tree a) (Context a)
