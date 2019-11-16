module SKI
    ( SKITerm(..)
    , reduction
    ) where
    
data SKITerm = SKIApply SKITerm SKITerm | S | K | I deriving (Show, Eq)

f $: a = SKIApply f a

reduction :: SKITerm -> SKITerm
reduction (((S $: x) $: y) $: z) = ((x $: z) $: (y $: z))
reduction ((K $: x) $: y) = x
reduction (I $: x) = x
reduction (function $: argument) = (reduction function) $: (reduction argument)
