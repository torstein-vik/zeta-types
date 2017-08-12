{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsing (
    module Parsing,
    ReadPrec,
    Read,
    readPrec,
    parens,
    step,
    (+++),
    (<++)
    
    
  ) where

import Text.Read as Read
import Control.Applicative as Applicative
import qualified Text.ParserCombinators.ReadP as ReadP

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