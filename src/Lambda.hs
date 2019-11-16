module Lambda
    ( lamapp
    , lamabs
    , lamvar
    ) where

data LambdaTerm = LambdaApply LambdaTerm LambdaTerm | LambdaAbstract String LambdaTerm | LambdaVariable String

lamapp = LambdaApply
lamabs = LambdaAbstract
lamvar = LambdaVariable
