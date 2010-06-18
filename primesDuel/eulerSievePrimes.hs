import SimpleTest
import Data.Numbers.Primes

main :: IO ()
main = putStrLn $ runAsserts tests

--primesUpTo :: (Integral a) => a -> [a]
primesUpTo a = takeWhile (<=a) primes
--primesUpTo upTo = primes [] initialPool
--  where 
--    initialPool = 2 : [3, 5 .. upTo]
--    primes ps [] = reverse ps
--    primes ps pool = primes (head shrinkPool : ps) (tail shrinkPool)
--      where 
--        shrinkPool = diff pool scratched
--        scratched = takeWhile (<=upTo) . map (*h) $ pool
--        h = head pool

-- keep first list, filter out second list's elements
diff :: (Ord a) => [a] -> [a] -> [a]
diff allA@(a:as) allB@(b:bs) =
  case compare a b of
    LT -> a : diff as allB
    EQ -> diff as bs
    GT -> diff allA bs
diff as [] = as
diff [] bs = []


-- tests
-- =====

tests :: [AssertResult]
tests = 
  [ assertEq "matches known primes up to 271" 
    primesList (primesUpTo 271)
  , assertEq "matches largest prime under 2,000,000" 
    1999993 (last $ primesUpTo 2000000) 
  ]

primesList :: (Integral a) => [a]
primesList = 
  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,
   61,67,71,73,79,83,89,97,101,103,107,109,113,127,
   131,137,139,149,151,157,163,167,173,179,181,191,
   193,197,199,211,223,227,229,233,239,241,251,257,
   263,269,271]
