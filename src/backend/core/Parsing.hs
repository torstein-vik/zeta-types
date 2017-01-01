{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsing where

import Text.Read as Read
import Control.Applicative as Applicative
import Data.Proxy
import GHC.TypeLits
import qualified Text.ParserCombinators.ReadP as ReadP


import TannakianSymbols as TS
import Polynomials as Polynomials

instance (Ord m, Read m) => Read (TS m) where
    readPrec = parens $ do 
                        x <- readSet
                        expectC '/'
                        y <- readSet
                        return $ (unflatten x) TS.- (unflatten y)
                         
            
            where
                readSet :: (Read m) => ReadPrec ([m])
                readSet = (expectC 'Ø' >> return []) 
                
                                +++ between '{' '}' (do 
                                            x <- split ',' readPrec
                                            return x)
                            
                unflatten :: (Ord m) => [m] -> TS m 
                unflatten l = listSymbol $ zip l (repeat 1)

-- TODO: Look into parsing things like "1 - p", as opposed to "1 + -p"
-- TODO: Fix ambiguous parsing of PolynomialY (PolynomialX Integer)
instance (CMult m, CAdd m, Read m, KnownSymbol s) => Read (Polynomial m s) where
    readPrec = parens $ do
                        terms <- split '+' (parens (readTerm $ symbolVal (Proxy :: Proxy s)))
                        return $ foldr (TS.+) zero (fmap createCoeffs terms)
                        
                        
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
                                                    return (TS.negate e, 1)
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
                                                    return (TS.negate e,n)

nospaceExpectC :: Char -> ReadPrec Char
nospaceExpectC c = lift (ReadP.satisfy (\x -> x == c))

expectC :: Char -> ReadPrec Char
expectC c = do
            many (nospaceExpectC ' ')
            nospaceExpectC c
            
-- Maybe use nospaceExpectC instead? possibly...
expectS :: String -> ReadPrec String
expectS s = foldr (>>) (return "") (fmap expectC s)
            
between :: Char -> Char -> ReadPrec a -> ReadPrec a
between c1 c2 r = do
                expectC c1
                x <- r
                expectC c2
                return x

split :: (Read a) => Char -> ReadPrec a -> ReadPrec [a]
split c read = do 
                y <- step read
                x <- excess
                return (y:x)
                
        where
            excess = many $ do 
                expectC c
                x <- step read
                return x