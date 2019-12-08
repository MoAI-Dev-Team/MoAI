{-# LANGUAGE DeriveDataTypeable #-}
module SKI
    ( SKITerm(..)
    , reduce
    ) where
    
import Data.Typeable
import Data.Data

data SKITerm = Apply SKITerm SKITerm 
             | S 
             | K 
             | I 
             deriving (Typeable, Data, Show, Eq)
