{-# LANGUAGE NoImplicitPrelude #-}

module Algebra where

import qualified Prelude
import Prelude hiding ((+),(-),(*),(^),(/), negate)


-----------------------------------------------------------------------------
-- Haskell Num doesn't have the semantics we need so this small module
-- contains some of the needed classes.
-----------------------------------------------------------------------------


-- TODO: Set up order of operations with infix stuff

-- Minimal definition: (+), zero, (negate | (-))
class CAdd r where
    (+), (-) :: r -> r -> r
    negate :: r -> r
    zero :: r

    a - b = a + (negate b)
    negate x = zero - x

-- Minimal definition: (*), e
class CMult m where
    (*) :: m -> m -> m
    e :: m
    (^) :: m -> Int -> m

    x^n = foldr (*) e (replicate n x)

-- Minimal definition (invert | (/))
class CMult m => CGroup m where
    (/) :: m -> m -> m
    invert :: m -> m

    a / b = a * (invert b)
    invert x = e / x

-- Minimal definition: psi
-- TODO: Memoize lambda for speed
class (CAdd r, CMult r, CIntDiv r) => LambdaRing r where
    psi :: Int -> r -> r
    lambda :: Int -> r -> r

    lambda 0 x = e
    lambda 1 x = x
    lambda n x = (if odd n then id else negate) $ (foldr (+) zero (map (\i -> (if odd i then negate else id) $ (lambda i x * psi (n - i) x)) [0..n-1])) /: n

-- Basically a Q-algebra, division by integer
class CIntDiv m where
    (/:) :: m -> Int -> m


-----------------------------------------------------------------------------
-- Instances for common data-types
-----------------------------------------------------------------------------



instance CAdd Integer where
    a + b = a Prelude.+ b
    a - b = a Prelude.- b
    zero = 0;

instance CMult Integer where
    a * b = a Prelude.* b
    x^n = x Prelude.^n
    e = 1

instance CIntDiv Integer where
    n /: m = n `quot` (toInteger m)

instance CAdd Int where
    a + b = a Prelude.+ b
    a - b = a Prelude.- b
    zero = 0;

instance CMult Int where
    a * b = a Prelude.* b
    x^n = x Prelude.^n
    e = 1

instance CIntDiv Int where
    (/:) = quot


