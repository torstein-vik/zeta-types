{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Algebra where

import Control.Monad
import Data.Maybe

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
    (^) :: m -> Integer -> m

    x^n = foldr (*) e (replicate (fromInteger n) x)
    
-- Minimal definition: ()
class (CMult m, CAdd m) => CZModule m where
    structureMap :: Int -> m
    one :: m
    (*#) :: m -> Int -> m
    
    one = structureMap 1
    structureMap n = one *# n
    
--instance (CMult m, CAdd m) => CZAlgebra m

-- Minimal definition (invert | (/))
class CMult m => CGroup m where
    (/) :: m -> m -> m
    invert :: m -> m

    a / b = a * (invert b)
    invert x = e / x

-- Minimal definition: psi
-- TODO: Memoize lambda for speed
class (CAdd r, CMult r, CPartialQModule r) => LambdaRing r where
    psi :: Integer -> r -> r
    lambda :: Integer -> r -> r

    lambda 0 x = e
    lambda 1 x = x
    lambda n x = (if odd n then id else negate) . fromJust $ (foldr (+) zero (map (\i -> (if odd i then negate else id) $ (lambda i x * psi (n - i) x)) [0..n-1])) /# n

-- Basically a Q-algebra, division by integer
class CPartialQModule m where
    (/#) :: m -> Integer -> Maybe m



-----------------------------------------------------------------------------
-- Instances for type constructors
-----------------------------------------------------------------------------



instance (CAdd m, CAdd n) => CAdd (m, n) where
    (a, b) + (c, d) = (a + c, b + d)
    (a, b) - (c, d) = (a - c, b - d)
    negate   (a, b) = (negate a, negate b)
    zero            = (zero, zero)

instance (CMult m, CMult n) => CMult (m, n) where
    (a, b) * (c, d) = (a * c, b * d)
    (a, b) ^ n      = (a ^ n, b ^ n)
    e               = (e, e)

instance (CGroup m, CGroup n) => CGroup (m, n) where
    (a, b) / (c, d) = (a / c, b / d)
    invert (a, b)   = (invert a, invert b)

instance (CZModule m, CZModule n) => CZModule (m, n) where
    (a, b) *# n    = (a *# n, b *# n)
    structureMap n = (structureMap n, structureMap n)
    one            = (one, one)

instance (CPartialQModule n, CPartialQModule m) => CPartialQModule (n, m) where
    (a, b) /# n    = liftM2 (\a -> \b -> (a, b)) (a /# n) (b /# n)


    
-----------------------------------------------------------------------------
-- Instances for common data-types
-----------------------------------------------------------------------------



instance CAdd Int where
    (+) = (Prelude.+)
    (-) = (Prelude.-)
    zero = fromIntegral 0

instance CMult Int where
    (*) = (Prelude.*)
    (^) = (Prelude.^)
    e = fromIntegral 1

instance CZModule Int where
    structureMap = fromIntegral
    n *# m = n * (fromIntegral m)

instance CPartialQModule Int where
    a /# n = let (quot, rem) = a `divMod` (fromInteger n) in if rem == 0 then Just quot else Nothing

instance CAdd Integer where
    (+) = (Prelude.+)
    (-) = (Prelude.-)
    zero = fromIntegral 0

instance CMult Integer where
    (*) = (Prelude.*)
    (^) = (Prelude.^)
    e = fromIntegral 1

instance CZModule Integer where
    structureMap = fromIntegral
    n *# m = n * (fromIntegral m)

instance CPartialQModule Integer where
    a /# n = let (quot, rem) = a `divMod` (fromInteger n) in if rem == 0 then Just quot else Nothing