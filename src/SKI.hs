module SKI
    ( SKITerm(..)
    , reduction
    ) where
    
data SKITerm = Apply SKITerm SKITerm | S | K | I deriving (Show, Eq)

reduction :: SKITerm -> SKITerm
reduction (S `Apply` x `Apply` y `Apply` z) = x `Apply` z `Apply` (y `Apply` z)
reduction (K `Apply` x `Apply` _) = x
reduction (I `Apply` x) = x
reduction (function `Apply` argument) = reduction function `Apply` reduction argument
