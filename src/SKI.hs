module SKI
    ( SKITerm(..)
    ( where
    
data SKITerm = SKITerm :$ SKITerm | S | K | I deriving (Show, Eq)
