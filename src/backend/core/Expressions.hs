module Expressions where

import TannakianSymbols as TS
import qualified Prelude
import Prelude hiding ((+),(-),(*),(^), negate)

data Expression m = Value (m)
    | (Expression m) :+: (Expression m)
    | (Expression m) :-: (Expression m) 
    | (Expression m) :*: (Expression m)
    | (Expression m) :^: (Int)
    | Psi (Int) (Expression m)

eval :: (CAdd m, CMult m, LambdaRing m) => Expression m -> m
eval (Value x) = x
eval (x :+: y) = (eval x) + (eval y)
eval (x :-: y) = (eval x) - (eval y)
eval (x :*: y) = (eval x) * (eval y)
eval (x :^: n) = (eval x) ^ n
eval (Psi n x) = psi n (eval x)

instance (Ord m, Show m) => Show (Expression m) where
    show (Value x) = show x
    show (x :+: y) = showBinary x y '+' 
    show (x :-: y) = showBinary x y '-'
    show (x :*: y) = showBinary x y '*'
    show (x :^: n) = show x ++ "^" ++ show n
    show (Psi n x) = "psi^" ++ show n ++ "(" ++ show x ++ ")"

showBinary :: (Ord m, Show m) => Expression m -> Expression m -> Char -> [Char]
showBinary x y c = show x ++ [' ',c,' '] ++ show y

complexityMeasure :: (Ord m) => Expression m -> Int
complexityMeasure (Value x) = undefined

binaries = [(:+:),(:-:),(:*:)]
unaries = [(Psi 2), (Psi 3)]

-- Expression generation. Values to use -> amount of binaries -> max amount of unaries -> All expressions
genExprTS :: [TS m] -> Integer -> Integer -> [Expression (TS m)]
genExprTS values = genExpr (map Value values)

genExpr :: [Expression m] -> Integer -> Integer -> [Expression m]
genExpr values 0 0 = values
genExpr values b 0 = do
                    op <- binaries
                    lhs <- (genExpr values (b - 1) 0)
                    rhs <- (genExpr values 0 0)
                    return $ op lhs rhs
genExpr values 0 u = do
                    expr <- genExpr values 0 (u - 1)
                    u <- unaries
                    return $ u $ expr
genExpr values b u = do
                    op <- binaries
                    tu <- [0, 1]
                    ru <- [0..(u - tu)]
                    lu <- [0..(u - tu - ru)]
                    lhs <- genExpr values (b - 1) ru
                    rhs <- genExpr values 0 lu
                    u <- if tu == 1 then unaries else [id]
                    return $ u $ op lhs rhs



---- DYNAMIC PRINTING PART ---- PROBABLY DELETE

-- Akin to writer monad, not sure how it's different to be honest...
{-data DynamicPrinter m = DynamicPrinterRest (Expression m) String [String]

-- Do we need this???

instance Functor (DynamicPrinter) where
    fmap f (DynamicPrinterRest x c rest) = DynamicPrinterRest (f x) c rest
    
instance Applicative (DynamicPrinter) where
    pure x = DynamicPrinterRest x "n" (groupBy (\_ _ -> False) ("mabcdexy"))
    _ <*> (DynamicPrinterRest x c chars) = DynamicPrinterRest (x) c chars -- is this right ???

instance Monad (DynamicPrinter) where
    --f =<< (DynamicPrinterRest x chars) = (f x)-}
