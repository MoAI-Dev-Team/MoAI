module SKI
    ( SKITerm(..)
    ) where
    
data SKITerm = SKITerm :$ SKITerm | S | K | I deriving (Show, Eq)

reduction :: SKITerm -> Either SKITrem SKITerm
reduction (((S :$ x) :$ y) :$ z) =  Left ((x :$ z) :$ (y :$ z))
reduction ((K :$ x) :$ y) = Left x
reduction (I :$ x) = Left x
reduction (function :$ argument) = (reduction function) :$ (reduction argument)
