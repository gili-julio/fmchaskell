module Nat where

import Prelude
  hiding ((-), last, (<), (>), (>=), (<=), compare, init, isPrefixOf, maximum, minimum, drop, take, enumFromTo, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)

data Nat = O | S Nat
    deriving ( Eq , Show )

(+) :: Nat -> Nat -> Nat
m + O = m
m + (S n) = S (m + n)

(*) :: Nat -> Nat -> Nat
m * O = O
m * (S n) = m + (m * n)

(^) :: Nat -> Nat -> Nat
m ^ O = S O
m ^ (S n) = m * (m ^ n)

double :: Nat -> Nat
double O = O
double (S n) = S(S(double n))

pred :: Nat -> Nat
pred O = O
pred (S n) = n

(-) :: Nat -> Nat -> Nat
m - O = m
O - m = O
(S m) - (S n) = m - n

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

min :: Nat -> Nat -> Nat
min O n = O
min m O = O
min (S m) (S n) = S(min m n)

max :: Nat -> Nat -> Nat
max O n = n
max m O = m
max (S m) (S n) = S(max m n)