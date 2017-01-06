module Util (
    paddedZipWith,
    paddedZipWith2,
    paddedZip,
    paddedZip2
  ) where

-----------------------------------------------------------------------------
-- This module contains some useful tools,
-- for now only a padded zip
-----------------------------------------------------------------------------

paddedZipWith :: (a -> a -> c) -> [a] -> [a] -> a -> [c]
paddedZipWith f a1 a2 a = let max = maximum (map length [a1, a2]) in 
    zipWith f (a1 ++ (replicate (max - length a1) a)) (a2 ++ (replicate (max - length a2) a))

paddedZipWith2 :: (a -> b -> c) -> [a] -> [b] -> a -> b -> [c]
paddedZipWith2 f al bl a b = let max = maximum [length al, length bl] in 
    zipWith f (al ++ (replicate (max - length al) a)) (bl ++ (replicate (max - length bl) b))

paddedZip = paddedZipWith (\a -> \b -> [(a, b)])
paddedZip2 = paddedZipWith2 (\a -> \b -> [(a, b)])