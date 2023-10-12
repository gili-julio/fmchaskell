module Bool where

import Prelude
    hiding ((-), last, compare, (<), (<=), (>), (>=), init, isPrefixOf, maximum, minimum, drop, take, enumFromTo, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem, Bool, True, False)

import Types ( Nat(..), Bool(..) )

ifthen :: Bool -> Nat -> Nat -> Nat
ifthen True m n = m
ifthen False m n = n

iff :: Bool -> (Nat, Nat) -> (Nat, Nat) -> (Nat, Nat)
iff True m n = m
iff False m n = n

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

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S(S m)) = ev m

od :: Nat -> Bool
od O = False
od (S O) = True
od (S(S m)) = od m

isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S(S(S m))) = isMul3 m
isMul3 m = False

isZero :: Nat -> Bool
isZero O = True
isZero m = False
