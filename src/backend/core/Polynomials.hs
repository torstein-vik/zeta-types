{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Polynomials where

import GHC.TypeLits
import Data.Proxy

import Control.Monad as Monad (liftM2)
import Data.List as List (intercalate)

import qualified Prelude
import Prelude hiding ((+),(-),(*),(^),(/), negate)

import Algebra
import Parsing

-----------------------------------------------------------------------------
-- This module is dedicated to working with polynomials
-- of any base ring and variable-letter.
-----------------------------------------------------------------------------

data Polynomial :: * -> Symbol -> * where 
    Coeffs :: [m] -> Polynomial m s deriving Ord

type PolynomialX m = Polynomial m "x"
type PolynomialY m = Polynomial m "y"
type PolynomialP m = Polynomial m "p"
type PolynomialT m = Polynomial m "t"

type PolyIntX = Polynomial Int "x"
type PolyIntY = Polynomial Int "y"
type PolyIntP = Polynomial Int "p"
type PolyIntT = Polynomial Int "t"

outfromCoeffs (Coeffs x) = x
intoCoeffs x = Coeffs x

paddedZipWith :: (a -> a -> c) -> [a] -> [a] -> a -> [c]
paddedZipWith f a1 a2 a = let max = maximum (map length [a1, a2]) in 
    zipWith f (a1 ++ (replicate (max - length a1) a)) (a2 ++ (replicate (max - length a2) a))

paddedZipWith2 :: (a -> b -> c) -> [a] -> [b] -> a -> b -> [c]
paddedZipWith2 f al bl a b = let max = maximum [length al, length bl] in 
    zipWith f (al ++ (replicate (max - length al) a)) (bl ++ (replicate (max - length bl) b))

paddedZip = paddedZipWith (\a -> \b -> [(a, b)])
paddedZip2 = paddedZipWith2 (\a -> \b -> [(a, b)])

instance (CAdd m) => CAdd (Polynomial m s) where
    zero = Coeffs []
    Coeffs x + Coeffs y = Coeffs $ paddedZipWith (+) x y zero
    negate (Coeffs x) = Coeffs $ map negate x
    
instance (CAdd m, CMult m) => CMult (Polynomial m s) where
    e = Coeffs[e]
    Coeffs x * Coeffs y = foldr (+) zero . fmap (\(n, x) -> Coeffs $ replicate n zero ++ [x]) $ liftM2 (\(n,x) (m,y) -> (n + m, x * y)) (zip [0..] x) (zip [0..] y)

instance (CAdd m, Eq m) => Eq (Polynomial m s) where
    Coeffs a == Coeffs b = and $ paddedZipWith (==) a b zero

instance (CAdd m, CMult m, Eq m, Show m, KnownSymbol s) => Show (Polynomial m s) where
    {-show (Coeffs []) = "0"
    show x | x == zero = "0" 
    show (Coeffs [a]) = if a == zero then "!!!!!!" else show a
    show (Coeffs [a, b]) = show (Coeffs [a]) ++ " + " ++ show (Coeffs [b]) ++ variableString
    show (Coeffs (a:b:xs)) = (show $ Coeffs [a, b]) ++ foldl (\a b -> a ++ " + " ++ b) "" (map (\(x, n) -> (show x) ++ variableString ++ "^" ++ show n) (zip xs [2..]))-}
    
    show (Coeffs []) = "0"
    show (Coeffs coeffs) = intercalate " + " $ do
                            term <- zip coeffs [0..]
                            if fst term /= zero then
                                return $ showTerm term (symbolVal (Proxy :: Proxy s))
                            else []
    
        where
            asCoeff m | m == e = ""
                      | m == negate e = "-"
                      | otherwise = show m
            showTerm :: (CMult m, CAdd m, Eq m, Show m) => (m, Integer) -> String -> String
            showTerm (m, 0) variable = show m
            showTerm (m, 1) variable = asCoeff m ++ variable
            showTerm (m, n) variable = asCoeff m ++ variable ++ "^" ++ show n



-- TODO: Look into parsing things like "1 - p", as opposed to "1 + -p"
-- TODO: Fix ambiguous parsing of PolynomialY (PolynomialX Integer)
instance (CMult m, CAdd m, Read m, KnownSymbol s) => Read (Polynomial m s) where
    readPrec = parens $ do
                        terms <- split '+' (parens (readTerm $ symbolVal (Proxy :: Proxy s)))
                        return $ foldr (+) zero (fmap createCoeffs terms)
                        
                        
            where
                createCoeffs :: (CAdd m) => (m, Int) -> Polynomial m s
                createCoeffs (x, n) = Coeffs $ (replicate n zero) ++ [x]
                
                readTerm :: (Read m, CMult m, CAdd m) => String -> ReadPrec (m, Int)
                readTerm variable = zeroDegreeTerm
                        +++ (firstDegreeTerm variable  <++ nUnitFirstDegreeTerm variable  <++ unitFirstDegreeTerm variable) 
                        +++ (nDegreeTerm variable      <++ nUnitNDegreeTerm variable      <++ unitNDegreeTerm variable    )
                
                zeroDegreeTerm          :: (Read m, CMult m)           => ReadPrec (m, Int)
                firstDegreeTerm         :: (Read m, CMult m)           => String -> ReadPrec (m, Int)
                unitFirstDegreeTerm     :: (Read m, CMult m)           => String -> ReadPrec (m, Int)
                nUnitFirstDegreeTerm    :: (Read m, CMult m, CAdd m)   => String -> ReadPrec (m, Int)
                nDegreeTerm             :: (Read m, CMult m)           => String -> ReadPrec (m, Int)
                unitNDegreeTerm         :: (Read m, CMult m)           => String -> ReadPrec (m, Int)
                nUnitNDegreeTerm        :: (Read m, CMult m, CAdd m)   => String -> ReadPrec (m, Int)
                
                zeroDegreeTerm = do 
                                                    x <- parens $ step readPrec
                                                    return (x, 0)
                firstDegreeTerm variable = do
                                                    x <- parens $ step readPrec
                                                    expectS variable
                                                    return (x, 1)
                unitFirstDegreeTerm variable = do
                                                    expectS variable
                                                    return (e, 1)
                nUnitFirstDegreeTerm variable = do
                                                    expectC '-'
                                                    expectS variable
                                                    return (negate e, 1)
                nDegreeTerm variable = do 
                                                    x <- parens $ step readPrec
                                                    expectS variable
                                                    expectC '^'
                                                    n <- readPrec
                                                    return (x,n)
                unitNDegreeTerm variable = do
                                                    expectS variable
                                                    expectC '^'
                                                    n <- readPrec
                                                    return (e,n)
                nUnitNDegreeTerm variable = do
                                                    expectC '-'
                                                    expectS variable
                                                    expectC '^'
                                                    n <- readPrec
                                                    return (negate e,n)



{-instance Functor Polynomial where
    fmap f (Coeffs c) = Coeffs $ map f c 

instance Applicative Polynomial where
    pure x = Coeffs [x]
    (<*>) = ap

instance Monad Polynomial where
    Coeffs x >>= f = foldr (TS.+) zero (map f x)-}

{-instance (Read m, CAdd m, CMult m) => Read (Polynomial m) where
    readPrec = 
      parens 
      ( x <- readPrec
        xs <- (readNext)
        return 
      )
      where
        readNext = do
                x <- reset readPrec
                expectP (L.Ident variableString)
                xs <- readRest
                return x + xs-}

{-instance (Read m, CAdd m, CMult m) => Read (Polynomial m) where
  readPrec = 
    parens
    ( do L.String x <- lexP
         return $ readPoly x
    )-}

--newtype Polynomial m = Polynomial m

{-instance (Read m, CAdd m, CMult m) => ProtoRead (Polynomial m) where
    pRead = readPoly

    -- TODO: Fix so that 1 + (2 + 3)p is properly parsed
readPoly :: (Read m, CAdd m, CMult m) => String -> Polynomial m
readPoly s = (foldr (TS.+) (zero) (map readTerm splitString)) where
    partialRead s   | and $ map (' '==) s = e
                    | otherwise = read s
    splitString = splitRegex (mkRegex "\\+") s
    readTerm string | (isInfixOf (variableString ++ "^") string) = (\[v, n] -> Coeffs $ (replicate (read n) zero) ++ [partialRead v]) $ (splitRegex (mkRegex (variableString ++ "\\^")) string)
                    | (isInfixOf variableString string) = Coeffs [zero, partialRead $ (splitRegex (mkRegex variableString) string) !! 0]
                    | otherwise = Coeffs [partialRead string]-}