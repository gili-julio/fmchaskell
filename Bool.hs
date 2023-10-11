module Bool where
import Prelude
    hiding (True, False, Bool, (<), (<=), (>), (>=))
import Nat

data Bool = True | False
        deriving(Eq, Show)

ifthen :: Bool -> Nat -> Nat -> Nat
ifthen True m n = m
ifthen False m n = n

(<) :: Nat -> Nat -> Bool
O < (S n) = True
m < O = False
(S m) < (S n) = m < n

(<=) :: Nat -> Nat -> Bool
O <= n = True
(S m) <= O = False
(S m) <= (S n) = m <= n

(>) :: Nat -> Nat -> Bool
O > n = False
(S m) > O = True
(S m) > (S n) = m > n

(>=) :: Nat -> Nat -> Bool
O >= (S n) = False
m >= O = True
(S m) >= (S n) = m >= n
