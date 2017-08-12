{-# LANGUAGE NoImplicitPrelude #-}


module ZetaTypeOfRings(
    module ZetaTypeOfRings
) where

import TannakianSymbols
import Parsing

import Data.List

import qualified Prelude
import Prelude hiding ((+),(-),(*),(^),(/), negate)

data Zmod = Zmod [Integer]

cleanupZM :: Zmod -> Zmod
cleanupZM (Zmod x) = Zmod . combine . reverse . sort $ x where
                        combine [] = [] 
                        combine (x:[]) = if x==1 then [] else [x]
                        combine (x:y:m) = let (a, b) = (gcd x y, lcm x y) in 
                          if a == 1 then combine (b : m) else b : combine (a : m)

instance CMult Zmod where
    e = Zmod []
    Zmod a * Zmod b = cleanupZM . Zmod $ a ++ b

instance Show Zmod where
    show (Zmod []) = "0"
    show (Zmod n) = intercalate " + " $ fmap (("Z/" ++) . show) n

instance Eq Zmod where
    a == b = let (Zmod x, Zmod y) = (cleanupZM a, cleanupZM b) in x == y

type Ringmonoid = TS Zmod

instance Read Zmod where
    readPrec = fmap (cleanupZM . Zmod) $ split '+' (expectS "Z/" >> readPrec)
                    