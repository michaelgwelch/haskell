module FloatAddition where

import Group

instance Group Float where
    identity = 0
    operation = (+)
    inverse = (0-)
