{-# LANGUAGE DeriveDataTypeable #-}
module SKI
    ( SKITerm(..)
    , reduction
    ) where
    
import Data.Typeable
import Data.Data

data SKITerm = Apply SKITerm SKITerm | S | K | I deriving (Typeable, Data, Show, Eq)

reduction :: SKITerm -> SKITerm
reduction (S `Apply` x `Apply` y `Apply` z) = x `Apply` z `Apply` (y `Apply` z)
reduction (K `Apply` x `Apply` _) = x
reduction (I `Apply` x) = x
reduction (f `Apply` x) = 
  if freducted /= f
  then freducted `Apply` x
  else f `Apply` xreducted
  where
    freducted = reduction f
    xreducted = reduction x
reduction x = x
