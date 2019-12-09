{-# LANGUAGE DeriveDataTypeable #-}
module Lambda
    ( LambdaTerm(..)
    , Reducible(..)
    ) where

import Data.Typeable
import Data.Data

import Types

data LambdaTerm
    = Apply LambdaTerm LambdaTerm
    | Abstract String LambdaTerm
    | Variable String
    deriving (Typeable, Data, Show, Eq)

instance Reducible LambdaTerm where
    reduce ((Abstract s x) `Apply` y) = replace s y x
        where
            replace :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
            replace s x (y `Apply` z) = replace s x y `Apply` replace s x z
            replace s1 x (Abstract s2 y) = Abstract s2 $ replace s1 x y
            replace s1 x y@(Variable s2) = if s1 == s2 then x else y
    reduce (f `Apply` x) = 
        if freducted /= f
        then freducted `Apply` x
        else f `Apply` xreducted
        where
            freducted = reduce f
            xreducted = reduce x
    reduce x = x
