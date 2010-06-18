module Main where

-- tests parallel programs in haskell

import Data.Maybe
import Control.Parallel.Strategies
import Control.Parallel
import Data.List

targets       = replicate 10 (floor $ 10 ** 5)
makeSeries n  = [1..n]
robustProduct = foldl' (*) 1

main = print $ (parMap rwhnf) ((*0) . robustProduct . makeSeries) targets


{-
 - the original

module Main where
import Data.Maybe
import Control.Parallel.Strategies
import Control.Parallel

factors n = let candidates = [2..floor (sqrt (fromInteger n))]
            in catMaybes $ map (\x ->
                                      if n `mod` x == 0
                                      then Just (x, n `div` x)
                                      else Nothing) candidates
bigNums = [2000000000000..]
answer = (parMap rwhnf) (length . factors) (take 10 bigNums)

main = print answer

-}
