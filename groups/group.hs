module Group where


newtype Natural = Natural Int deriving (Eq, Show)

toNat :: Int -> Natural
toNat x | x < 0 = error "Naturals cannot be negative"
        | otherwise = Natural x

instance Num Natural where
    x + y = toNat (fromNat x + fromNat y)
    x - y = toNat (fromNat x - fromNat y)
    x * y = toNat (fromNat x * fromNat y)
    fromInteger = toNat . fromInteger
    abs = id
    signum = id

fromNat :: Natural -> Int
fromNat (Natural x) = x


data NaturalAddition = NA Natural deriving (Eq, Show)


class Eq a => Group a where
    identity :: a
    operation :: a -> a -> a
    inverse :: a -> a

    
instance Group NaturalAddition where
    identity = NA 0
    operation (NA x) (NA y) = NA (x+y)
    inverse (NA x) = NA (0-x)
    
assertCancellation :: Group a => a -> a -> a -> ()
assertCancellation a b c | (b==c) && (operation a b) == (operation a c) = ()
                         | (b==c) = error "Expected b equal to c" 
                         | otherwise = error "Cancellation fails"

assertIdentityCommutative :: Group a => a -> ()
assertIdentityCommutative x | (operation identity x) == (operation x identity)
                               && (operation identity x) == x  = ()
                            | otherwise = error "This didn't work"

assertIdentityInverse :: Group a => a -> ()
assertIdentityInverse x | (operation x (inverse x)) == (operation (inverse x) x)
                           && (operation x (inverse x) == identity) = ()
                        | otherwise = error "Wrong"
