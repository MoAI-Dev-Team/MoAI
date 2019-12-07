{-# LANGUAGE DeriveDataTypeable #-}
module Lambda
    ( LambdaTerm(..)
    , reduce
    ) where

import Data.Typeable
import Data.Data

data LambdaTerm
    = Apply LambdaTerm LambdaTerm
    | Abstract String LambdaTerm
    | Variable String
    deriving (Typeable, Data, Show, Eq)

replace :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
replace s x (y `Apply` z) = replace s x y `Apply` replace s x z
replace s1 x (Abstract s2 y) = Abstract s2 $ replace s1 x y
replace s1 x y@(Variable s2) = if s1 == s2 then x else y

reduce :: LambdaTerm -> LambdaTerm
reduce ((Abstract s x) `Apply` y) = replace s y x
reduce (f `Apply` x) = 
  if freducted /= f
  then freducted `Apply` x
  else f `Apply` xreducted
  where
    freducted = reduce f
    xreducted = reduce x
reduce x = x
