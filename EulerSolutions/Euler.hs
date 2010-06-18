import SimpleTest (AssertResult, assertEq, assertT, assertF, runAsserts)
import Data.List (isPrefixOf, foldl')
import Data.Numbers.Primes (primes)

main :: IO ()
main = putStrLn $ runAsserts asserts

asserts :: [AssertResult]
asserts = 
  [ assertT "fibs" ([1,2,3,5,8,13,21,34,55,89] `isPrefixOf` fibs)

  , assertT "isPalindrome yea" (isPalindrome 9009)
  , assertF "isPalindrome nay" (isPalindrome 9008)

  , assertEq "primeFactors" [2] (primeFactors 2)
  , assertEq "primeFactors" [2,3,7] (primeFactors 42)
  , assertEq "primeFactors" [2,5,7,11,13,17,19] (primeFactors 3233230)
  , assertEq "primeFactors" [41] (primeFactors 41)

  , assertT "divides yea" (4 `divides` 8)
  , assertF "divides nay" (3 `divides` 8)

  , assertEq "numOfFactors " 6 (numOfFactors 28)

  , assertEq "euler1"  233168       euler1
  , assertEq "euler2"  4613732      euler2
  , assertEq "euler3"  6857         euler3
  , assertEq "euler4"  906609       euler4
  , assertEq "euler5"  232792560    euler5
  , assertEq "euler6"  25164150     euler6
  , assertEq "euler7"  104743       euler7
  -- [ SKIP:  euler8  requires external data ]
  , assertEq "euler9"  31875000     euler9
  , assertEq "euler10" 142913828922 euler10
  -- [ SKIP:  euler11 requires external data ]
  , assertEq "euler12" 76576500     euler12
  ]

euler1 = sum [3,6..999] + sum [5,20..999] + sum [10,25..999]

euler2 = sum . filter even $ takeWhile (<4000000) fibs

euler3 = maximum $ primeFactors 600851475143 

euler4 = maximum [n| a<-[900..999], b<-[a..999], let n=a*b, isPalindrome n]

euler5 = product $ map p primes' 
  where 
    primes' = takeWhile (<=20) primes
    p n = n ^ floor (logBase d 20)
      where d = fromInteger n :: Double

euler6 = squareOfSum - sumOfSquares
  where 
    squareOfSum  = sum [1..100] ^ 2
    sumOfSquares = sum $ map (^2) [1..100]

euler7 = primes !! 10000

-- [ SKIP: euler8 requires external data ]

euler9 = mult . head . filter isPythTri $ map makeTriple aRange
  where 
    t      = 1000
    aRange = [1..t `div` 3]
    mult      (a,b,c) = a * b * c
    isPythTri (a,b,c) = a*a + b*b == c*c
    makeTriple a = (a,b,c)  -- made by solving a^2+b^2=c^2 and a+b+c=t
      where
        b = (2*a*t - t*t) `div` (2*a - 2*t)
        c = t - a - b

euler10 = sum $ takeWhile (<=2000000) primes

-- [ SKIP:  euler11 requires external data ]

euler12 = f 0 0
  where 
    f pos tri
      | numOfFactors tri > 500 = tri
      | otherwise              = f (pos + 1) (tri + pos)

fibs :: (Integral a) => [a]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

isPalindrome :: (Integral a) => a -> Bool
isPalindrome n = show n == reverse (show n)

primeFactors :: (Integral a) => a -> [a]
primeFactors n 
  | n  == firstFactor = [n]
  | otherwise         = firstFactor : primeFactors (n `div` firstFactor)
  where 
    firstFactor = head $ filter (`divides` n) range
    range       = 2 : [3,5..root n] ++ [n]
    root        = ceiling . sqrt . fromIntegral

divides :: (Integral a) => a -> a -> Bool
divides a b = floor divided == ceiling divided
  where divided = fromIntegral b / fromIntegral a

numOfFactors :: Integer -> Integer
numOfFactors 1 = 1
numOfFactors n = foldl' step 2 [2..root]
  where
    step acc m
      | m * m == n    = acc + 1
      | m `divides` n = acc + 2
      | otherwise     = acc
    root = floor . sqrt . fromIntegral $ n
