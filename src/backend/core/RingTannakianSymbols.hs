
module RingTannakianSymbols (
    module RingTannakianSymbols,
    module TannakianSymbols,
    module Algebra,
  ) where

import Control.Monad

import TannakianSymbols
import Algebra

trace :: (CAdd m) => TS m -> m
trace = wrap $ sum []