{-# LANGUAGE DeriveDataTypeable #-}
module SKI
    ( SKITerm(..)
    , Reducible(..)
    ) where
    
import Data.Typeable
import Data.Data

import Types

data SKITerm = Apply SKITerm SKITerm 
             | S 
             | K 
             | I 
             deriving (Typeable, Data, Show, Eq)

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
