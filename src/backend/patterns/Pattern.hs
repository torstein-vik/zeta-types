{-#LANGUAGE RankNTypes#-}
{-#LANGUAGE GADTs#-}
{-#LANGUAGE KindSignatures#-}
{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE MultiParamTypeClasses#-}

module Pattern(

  ) where

import Control.Monad
import Data.Maybe

data Pattern :: * -> * -> * -> * where
    Next   :: Pattern n i (n, i)
    Return :: m -> Pattern n i m
    Fail   :: Pattern n i m
    Rest   :: Pattern n i [(n, i)]
    Or     :: Pattern x i m -> Pattern x i n -> Pattern x i (Either m n)
    Bind   :: Pattern n i m -> (m -> Pattern n i a) -> Pattern n i a
    


instance Functor (Pattern n i) where
    fmap = liftM

instance Applicative (Pattern n i) where
    (<*>) = ap
    pure = return

instance Monad (Pattern n i) where
    return x = Return x
    pat >>= f = Bind pat f

run :: Pattern n i m -> [(n, i)] -> [(m, [(n, i)])]
run (Next)         (x:xs) = [(x, xs)]
run (Return y)     x      = [(y, x)]
run (Fail)         x      = []
run (Rest)         x      = [(x, [])]
run (Or pat1 pat2) x      = run (fmap Left pat1) x ++ run (fmap Right pat2) x
run (Bind pat f)   x      = do
                            (pat1, last) <- run pat x
                            z <- (run (f pat1) last)
                            return z

class Valuable m i n where
    valuation :: m -> i -> n

instance (Valuable a i n, Valuable b i n) => Valuable (Either a b) i n where
    valuation (Left a)  = valuation a
    valuation (Right b) = valuation b

filterPatterns :: (Eq i, Valuable m n i) => Pattern n i m -> [(n, i)] -> [m]
filterPatterns pat input = do 
                            (x, rest) <- run pat input
                            if and (do
                                    (i, y)    <- rest
                                    return (valuation x i == y)
                                    ) then return x else []




findPattern :: (Eq i, Valuable m n i) => Pattern n i m -> [(n, i)] -> Maybe m
findPattern pat input = case filterPatterns pat input of
                            pat:[] -> Just pat
                            _ -> Nothing

data Constant a = Constant a
data Linear a b = Linear a b

instance            Valuable (Constant n) a n where
    valuation (Constant a) _ = a

instance (Num n) => Valuable (Linear n n) n n  where
    valuation (Linear a b) x = a + b * x

instance (Show a)         => Show (Constant a) where
    show (Constant a) = show a
    
instance (Show a, Show b) => Show (Linear a b) where
    show (Linear a b) = show a ++ " + " ++ show b ++ "x"

constant :: (Eq m) => Pattern n m (Constant m)
constant = do
        (_, n) <- Next
        return $ Constant n

linear :: (Fractional m, Show m) => Pattern m m (Linear m m)
linear = do
        (n0, f0) <- Next
        (n1, f1) <- Next
        let b = (f0 - f1) / (n0 - n1)
        let a = f0 - b * n0
        return $ Linear a b