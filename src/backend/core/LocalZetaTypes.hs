{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

module LocalZetaTypes (
    LocalZetaType
  ) where

import GHC.TypeLits
import Data.Proxy
import Data.Complex
import TannakianSymbols as TS
import Polynomials as Poly

import Prelude hiding ((+),(-),(*),(^), negate)

-----------------------------------------------------------------------------

-- Semi-rule: lists should be lazy infinite. Thus compression/expansion won't fuck up precision
-- A Q-Algebra as the coefficient ring, a berlekamp-massey precision-value
data LocalZetaType :: * -> Nat -> * where
      PointCounts      :: [m] -> LocalZetaType m n
      BellCoeffs       :: [m] -> LocalZetaType m n
      Function         :: (PolynomialT m, PolynomialT m) -> LocalZetaType m n
      TannakianSymbol  :: TS m -> LocalZetaType m n
      deriving (Ord, Read, Show)

type LocalZetaTypeC10 = LocalZetaType (Complex Double) 10

toPointCounts :: LocalZetaType m n -> LocalZetaType m n
toBellCoeffs  :: LocalZetaType m n -> LocalZetaType m n
toFunction    :: LocalZetaType m n -> LocalZetaType m n
toSymbol      :: LocalZetaType m n -> LocalZetaType m n

toPointCounts (PointCounts l)     = PointCounts l
toPointCounts (BellCoeffs l)      = PointCounts . preBellDerivaite $ l
toPointCounts (Function f)        = toPointCounts . toSymbol $ (Function f) -- optimal?
toPointCounts (TannakianSymbol s) = PointCounts $ foldr (+) zero 


