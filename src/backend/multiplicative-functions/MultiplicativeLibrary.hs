{-# LANGUAGE DataKinds #-}

module MultiplicativeLibrary where

import Data.List as List
import Data.Maybe as Maybe
import Control.Monad as Monad
import Text.Read as Read

import TannakianSymbols
import Polynomials
import Expressions
import Parsing

symbols = map (read :: String -> TS (Polynomial Int "p")) ["{1}/{2}", "{p^3}/{1}", "{-1, p}/Ø", "{1, 1, 1, 1}/Ø", "{p}/Ø", "{1, 1, 1}/Ø", "{1, p}/Ø", "Ø/{1}", "{p^3}/{-1}", "{1, -1}/Ø", "{p^2}/Ø", "{p}/{-1}", "{p^3}/Ø", "{1, 1}/Ø", "{1}/{-1}", "{p^2}/{-1}", "{1}/Ø", "{-1}/Ø", "{p}/{1}", "{p^2}/{1}", "{1}/{-1p + 1}", "Ø/{-1}", "{1, p^3}/Ø", "{1, p^2}/Ø"]

genWithSymbols = Expressions.genExprTS symbols

type Prime = Integer

-- Design choice: exclude function? shorten name?
data MultiplicativeFunction m = Function m | FunctionException (Prime -> m) [Prime]

createMultFunc :: (Eq s) => s -> [(Prime, s)] -> MultiplicativeFunction s
createMultFunc x [] = Function x
createMultFunc x l = FunctionException (\p -> fromMaybe x (lookup p l)) (fmap fst l)

{-instance Functor MultiplicativeFunction where
    fmap f (Function m) = Function $ f m
    fmap f (FunctionException char exceptions) = FunctionException (f . char) exceptions

instance Applicative MultiplicativeFunction where
    pure x = Function x
    (<*>) = ap
    
instance Monad MultiplicativeFunction
    f =<< (Function x) = f x
    f =<< (FunctionException char exceptions) = 

-- multiplicative function and a way to print it at a certain variable

-- second parameter is name
createPrintableMultFunc :: MultiplicativeFunction m -> String -> PrintableMultFunc m
createPrintableMultFunc f s = MultFunc f (\x -> if x /= "" then s ++ "(" ++ x ++ ")" else s)

data PrintableMultFunc m = MultFunc (MultiplicativeFunction m) (String -> String)

dynamicToString :: Expression (PrintableMultFunc m) -> String
dynamicToString = \expr -> dynamicToStringWith expr "n" (liftM2 (flip (++)) ("":(map show [0..])) (groupBy (\_ _ -> False) ("mabcdexy")))

dynamicToStringWith :: Expression (PrintableMultFunc m) -> String -> [String] -> String
dynamicToStringWith (Value (MultFunc m f)) param chars = f param
dynamicToStringWith (x :+: y) param chars = undefined
{-eval (Value x) = x
eval (x :+: y) = (eval x) + (eval y)
eval (x :-: y) = (eval x) - (eval y)
eval (x :*: y) = (eval x) * (eval y)
eval (x :^: n) = (eval x) ^ n
eval (Psi n x) = psi n (eval x)-}-}