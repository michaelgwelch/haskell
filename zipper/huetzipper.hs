module HuetZipper where

data Tree a = Item a
            | Section [Tree a]

data Path a = Top
            | Node [Tree a] (Path a) [Tree a]

data Loc a = Loc (Tree a) (Path a)
    
