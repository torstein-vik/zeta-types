{-# LANGUAGE NoImplicitPrelude #-}


module RingTannakianSymbols (
    module TannakianSymbols,
    module Algebra,
    trace,
    pointCounts,
    bellCoeffs,
    bellDerivative
  ) where

import Control.Monad

import TannakianSymbols
import Algebra

import qualified Prelude
import Prelude hiding ((+),(-),(*),(^),(/), negate)

trace :: (CMult m, CAdd m) => TS m -> m
trace symbol = foldr (+) zero $ fmap (uncurry (*#)) $ unSymbol symbol

pointCounts :: (Eq m, CMult m, CAdd m) => TS m -> [m]
pointCounts m = [e] ++ do
                k <- [1..]
                return $ trace $ psi k m

bellCoeffs :: (Eq m, CMult m, CAdd m, CQAlgebra m) => TS m -> [m]
bellCoeffs = bellAntiDerivative . pointCounts

bellDerivative :: (CAdd m, CMult m, CZAlgebra m) => [m] -> [m]
bellDerivative lst = memo where
                        memoList = (memo !!)
                        memo = map (bd (memoList)) (fmap (\(i ,_) -> i) (zip [0..] lst))
                    
                        bd bdP 0 = e
                        bd bdP 1 = lst !! 1
                        bd bdP i = (lst !! i) *# i - foldr (+) zero (fmap (\j -> bdP (i - j) * (lst !! j)) [1..i - 1])
                        
bellAntiDerivative :: (CAdd m, CMult m, CQAlgebra m) => [m] -> [m]
bellAntiDerivative lst = memo where
                        memoList = (memo !!)
                        memo = map (bd (memoList)) (fmap (\(i ,_) -> i) (zip [0..] lst))
                    
                        bd bdP 0 = e
                        bd bdP 1 = lst !! 1
                        bd bdP i = ((lst !! i) + foldr (+) zero (fmap (\j -> bdP (i - j) * (lst !! j)) [1..i - 1])) /# i