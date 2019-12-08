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
