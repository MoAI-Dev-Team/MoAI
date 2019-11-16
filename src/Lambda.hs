module Lambda
    (
    ) where

data LambdaTerm = LambdaApply LambdaTerm LambdaTerm | LambdaAbstract String LambdaTerm | LambdaVariable String

lamapp = LambdaApply
lamabs = LambdaAbstract
lamvar = LambdaVariable
