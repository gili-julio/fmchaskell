module Nat where
import Prelude
    hiding ((-), last, compare, (<), (<=), (>), (>=), init, isPrefixOf, maximum, minimum, drop, take, enumFromTo, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem, Bool, True, False)
import Types
import Bool (iff, (<))

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

div :: (Nat, Nat) -> (Nat, Nat)
div (a, b) = iff (a < b) (O, a) (let (q', r') = div (a - b, b) in (S q', r'))

left :: (Nat, Nat) -> Nat
left (a, b) = a

right :: (Nat, Nat) -> Nat
right (a, b) = b

rem :: (Nat, Nat) -> Nat
rem (a, b) = right (div (a, b))

quot :: (Nat, Nat) -> Nat
quot (a, b) = left (div (a, b))