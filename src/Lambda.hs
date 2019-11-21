module Lambda
    ( LambdaTerm(..)
    , reduct
    ) where

data LambdaTerm = Apply LambdaTerm LambdaTerm | Abstract String LambdaTerm | Variable String

replace :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
replace s x (y `Apply` z) = replace s x y `Apply` replace s x z
replace s1 x (Abstract s2 y) = Abstract s2 y

reduct :: LambdaTerm -> LambdaTerm
reduct ((Abstract s x) `Apply` y) = replace s y x
reduct = id
