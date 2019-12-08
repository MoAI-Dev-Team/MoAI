module Types
    ( Reducible(..)
    ) where

class Eq a => Reducible a where
    reduce :: a -> a
    reduceAll :: a -> a
    reduceAll x = 
        if x == xreduced
        then x
        else reduceAll xreduced
        where
            xreduced = reduce x

