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

instance Reducible SKITerm where
    reduce (S `Apply` x `Apply` y `Apply` z) = x `Apply` z `Apply` (y `Apply` z)
    reduce (K `Apply` x `Apply` _) = x
    reduce (I `Apply` x) = x
    reduce (f `Apply` x) = 
        if freduced /= f
        then freduced `Apply` x
        else f `Apply` xreduced
        where
            freduced = reduce f
            xreduced = reduce x
    reduce x = x
