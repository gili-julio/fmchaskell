module Types where

data Nat = O | S Nat
    deriving ( Eq , Show )

data Bool = True | False
    deriving(Eq, Show)