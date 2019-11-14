module SKI
    ( SKITerm(..)
    ) where
    
data SKITerm = SKITerm :$ SKITerm | S | K | I deriving (Show, Eq)

reduction :: SKITerm -> SKITerm
reduction (((S :$ x) :$ y) :$ z) = (x :$ z) :$ (y :$ z)
reduction ((K :$ x) :$ y) = x
reduction (I :$ x) = x
reduction (function :$ argument) = (reduction function)
