
-- single linked list
data List a = Nil | Node a (List a)

-- A location has a value and a pointer next,
-- and a pointer previous.
data Loc a = Loc a (List a) (List a)
