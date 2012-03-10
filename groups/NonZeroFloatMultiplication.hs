module NonZeroFloatMult where

import Group

instance Group Float where
    identity = 1.0
    operation = (*)
    inverse x | x == 0 = error "Zero has no inverse with respect to *"
              | otherwise = (1/x)
