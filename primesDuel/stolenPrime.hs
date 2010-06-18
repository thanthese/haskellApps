import SimpleTest

main :: IO ()
main = putStrLn $ runAsserts tests

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : merge xt ys
    EQ -> x : merge xt yt
    GT -> y : merge xs yt

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : diff xt ys
    EQ -> diff xt yt
    GT -> diff xs yt

primes :: (Integral a) => [a]
primes = [2, 3, 5] ++ diff [7, 9 ..] nonprimes

nonprimes :: (Integral a) => [a]
nonprimes = foldr1 f . map g . tail $ primes
  where
    f (x:xt) ys = x : merge xt ys
    g p         = [ n * p | n <- [p, p + 2 ..]]


-- tests
-- =====

tests :: [AssertResult]
tests = 
  [ assertEq "matches known primes up to 271" 
    primesList (takeWhile (<=271) primes)
  , assertEq "matches largest prime under 2,000,000" 
    1999993 ((last . takeWhile (<2000000)) primes) 
  ]

primesList :: (Integral a) => [a]
primesList = 
  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,
   61,67,71,73,79,83,89,97,101,103,107,109,113,127,
   131,137,139,149,151,157,163,167,173,179,181,191,
   193,197,199,211,223,227,229,233,239,241,251,257,
   263,269,271]
