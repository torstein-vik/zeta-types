{-# LANGUAGE NoImplicitPrelude #-}

module Intervals (
    
  ) where

import Algebra

import qualified Prelude
import Prelude hiding ((+),(-),(*),(^),(/), negate)


-----------------------------------------------------------------------------
-- This module is for performing interval arithmetic 
-- without division or infinities. To be used for 
-- polynomial factorization at the very least.
-----------------------------------------------------------------------------

-- minimum and maximum
data Interval a = Interval a a deriving (Read, Show)

-----------------------------------------------------------------------------
-- Algebraic instances:
-----------------------------------------------------------------------------

-- Technically unrigorous

instance (CAdd a) => CAdd (Interval a) where
    Interval a b + Interval c d = Interval (a + c)    (b + d)
    negate (Interval a b)       = Interval (negate a) (negate b)
    zero                        = Interval (zero)     (zero)

instance (CMult a, Ord a) => CMult (Interval a) where
    Interval a b * Interval c d = let products = [a*c, a*d, b *c, b*d] in Interval (minimum products) (maximum products)
    e                           = Interval e e

-- Technically monadic, but in practice not computable or necessary.