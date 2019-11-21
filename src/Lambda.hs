module Lambda
    ( lamapp
    , lamabs
    , lamvar
    ) where

data LambdaTerm = Apply LambdaTerm LambdaTerm | Abstract String LambdaTerm | Variable String
