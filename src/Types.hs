module Types
    (
    ) where

import SKI

class Eq a => Reducible a where
    reduce :: a -> a
    reduceAll :: a -> a
    reduceAll x = 
        if x == xreduced
        then x
        else reduceAll xreduced
        where
            xreduced = reduce x

