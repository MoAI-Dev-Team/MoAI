module Lambda
    ( LambdaTerm(..)
    , reduction
    ) where

import Data.Typeable

data LambdaTerm
    = Apply LambdaTerm LambdaTerm
    | Abstract String LambdaTerm
    | Variable String
    deriving (Typeable, Data, Show, Eq)

replace :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
replace s x (y `Apply` z) = replace s x y `Apply` replace s x z
replace s1 x (Abstract s2 y) = Abstract s2 $ replace s1 x y
replace s1 x y@(Variable s2) = if s1 == s2 then x else y

reduction :: LambdaTerm -> LambdaTerm
reduction ((Abstract s x) `Apply` y) = replace s y x
reduction x = x
