{-# LANGUAGE NoImplicitPrelude #-}

module TannakianSymbols (
    module TannakianSymbols,
    module Algebra
  ) where

import Control.Monad

import Data.Graph.Inductive.Query.Monad

import qualified Prelude
import Prelude hiding ((+),(-),(*),(^),(/), negate)

import Algebra

-----------------------------------------------------------------------------
-- This module is dedicated to working with TannakianSymbols.
-- It would be best to define data TS m = Symbol (Map m Int),
-- but I want TS to be a Monad and Map always requires Ord m.
-----------------------------------------------------------------------------

data TS m = Symbol [(m, Int)]

-- Removes constructor
unSymbol :: TS m -> [(m, Int)]
unSymbol (Symbol x) = x

-- Wraps functions into TS
wrap  :: ([(m, Int)] -> [(n, Int)]) -> TS m -> TS n
wrap2 :: ([(m, Int)] -> [(n, Int)] -> [(o, Int)]) -> TS m -> TS n -> TS o
wrap3 :: ([(m, Int)] -> [(n, Int)] -> [(o, Int)] -> [(p, Int)]) -> TS m -> TS n -> TS o -> TS p
wrap  f (Symbol x) = Symbol $ f x
wrap2 f (Symbol x) (Symbol y) = Symbol $ f x y
wrap3 f (Symbol x) (Symbol y) (Symbol z) = Symbol $ f x y z

-- Performs f at each pair (m, Int) of the symbol
atEach :: ((m, Int) -> (n, Int)) -> TS m -> TS n
atEach f = wrap $ fmap f

-- Cleans up by adding terms with same type together (like 
-- (1, 1) and (1, 2)) and removing empty ones like (1, 0)
cleanup :: (Eq m) => TS m -> TS m
cleanup = wrap $ removeEmpty . addTogether where
    removeEmpty = filter  ((/= 0) . snd)
    addTogether = reverse . foldr (match) [] where
        match (x, n) ((y, m):xs) = if x == y then (y, n + m):xs else (y, m):(match (x, n) xs)
        match (x, n) [] = [(x, n)]

-----------------------------------------------------------------------------
-- Monad implementation:
-----------------------------------------------------------------------------


instance Functor TS where
    fmap f = atEach (mapFst f)

instance Applicative TS where
    (<*>) = ap
    pure = return

instance Monad TS where
    return x = Symbol [(x, 1)]
    Symbol x >>= f = Symbol . concat . map (\(a, n) -> map (mapSnd (*n)) (unSymbol . f $ a)) $ x

    
-----------------------------------------------------------------------------
-- Algebraic instances:
-----------------------------------------------------------------------------

instance CAdd (TS m) where
    (+)    = wrap2 (++)
    negate = (>>= (\x -> Symbol [(x, -1)]))
    zero   = Symbol []

instance (CMult m) => CMult (TS m) where
    (*) = liftM2 (*)
    e   = Symbol [(e, 1)]

-- We need Eq to clean up before quot-ing (if we don't we will get bad results)
instance (Eq m) => CIntDiv (TS m) where
    x /: n = mapSnd (`quot` n) `atEach` cleanup x
    
-- We need Eq because the definition of lambda requires CIntDiv (/:)
instance (Eq m, CMult m) => LambdaRing (TS m) where
    psi k = fmap (^k)

-----------------------------------------------------------------------------
-- Miscellaneous instances:
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- OBSOLETE CODE:
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

--charFunction :: TS m -> (m -> Int)
--charFunction (Symbol ts) n = Map.findWithDefault 0 n ts

--DELETE
{-listSymbol :: (Ord m) => [(m, Int)] -> TS(m)
listSymbol x = Symbol $ Map.fromListWith (Prelude.+) x

symbolList :: (Ord m) => TS(m) -> [(m, Int)]
symbolList (Symbol a) = Map.assocs a-}

--toMap :: TS m -> TannakianSymbol m
--toMap (Symbol a) = a

{-instance (Ord m) => CAdd (TS m) where
    Symbol a + Symbol b = Symbol $ Map.unionWith (Prelude.+) a b
    negate (Symbol a) = Symbol $ Map.map (Prelude.negate) a
    zero = Symbol $ Map.empty

instance (Ord m, CMult m) => CMult (TS m) where
    Symbol a * Symbol b = Symbol $ Map.fromListWith (Prelude.+) [(x*y,n Prelude.*m)| (x,n) <- Map.assocs a, (y,m) <- Map.assocs b]
    e = Symbol $ Map.fromList [(e, 1)]

instance (Ord m) => CIntDiv (TS m) where
    (Symbol x) / n = Symbol $ Map.map (`quot` n) x

instance (Ord m, CMult m) => LambdaRing (TS m) where
    psi k (Symbol x) = Symbol $ Map.mapKeysWith (Prelude.+) (^k) x

instance (Ord m, Eq m) => Eq (TS m) where
    x == y = all (\x -> (snd x) == 0) (Map.assocs $ toMap (x - y))

instance (Ord m, Show m) => Show (TS m) where
    show (Symbol ts) = showSet(upper) ++ "/" ++ showSet(lower) where
        upper = Map.filter ( > 0) ts
        lower = Map.map (Prelude.negate) $ Map.filter ( < 0) ts
        expand :: (Map m Int) -> [m]
        expand x = concat $ map (uncurry $ flip replicate) (Map.assocs x)
        showSet :: (Show m) => (Map m Int) -> String
        showSet x
            | Map.null x = "Ø"
            | otherwise = "{" ++ (intercalate ", " $ (map show (expand x))) ++ "}"-}


{-instance (Read m) => Read (TS m) where
    readPrec = parens 
        (do
            Read.expectP (L.Punc "{")
              (readNext False)
        )
        where 
            readRest negative = do 
                L.Punc c <- lexP
                case c of
                    "," -> readNext negative
                    "}" -> (if negative then return zero else expectP (L.Punc "/{")
                            readNext True)
                    _ -> pfail
            
            readNext negative = do 
                x <- parens readPrec
                xs <- readRest negative
                return x + xs-}

{-readTS :: (Ord m, Read m) => [Char] -> TS m
readTS string = (upperlower !! 0) - (upperlower !! 1) where
    upperlower = map readMap (splitRegex (mkRegex "/") string)
    readMap "Ø" = zero
    readMap s = listSymbol $ zip (map read (splitRegex (mkRegex ",") (filter (not . (liftM2 ((||)) (== '{') (== '}'))) s) )) (repeat 1)-}
