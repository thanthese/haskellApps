{-
  Project Euler: Haskell edition
  
  Started: 10.3.09
  Author: Stephen Mann
  Website: http://stephenmann.net
-}


{-
  Testing framework
  =================
-}


main :: IO ()
main = do
  putStrLn "Test all solutions:" 
--  putStrLn $ testStr 1  euler1  233168
--  putStrLn $ testStr 2  euler2  4613732
--  putStrLn $ testStr 3  euler3  6857 
--  putStrLn $ testStr 4  euler4  906609
--  putStrLn $ testStr 5  euler5  232792560 
--  putStrLn $ testStr 6  euler6  25164150 
--  putStrLn $ testStr 7  euler7  104743 
--  putStrLn $ testStr 8  euler8  40824
--  putStrLn $ testStr 9  euler9  31875000 
--  putStrLn $ testStr 10 euler10 142913828922
--  putStrLn $ testStr 11 euler11 70600674
--  putStrLn $ testStr 12 euler12 76576500
  putStrLn "primeFactors" 
  putStrLn $ show assert

assert :: Bool
assert = [2,5,7,11,13,17,19] == (primeFactorization 3233230) 

testStr :: (Integral a) => Int -> a -> a -> String
testStr num fn answer = result (fn == answer)   
  ++ "euler " ++ numStr num ++ ": "
  ++ "fn result = " ++ show fn
  where
    result True = "  pass "
    result False = "** FAIL ** "
    numStr s
      | s < 10    = show num ++ " "
      | otherwise = show num


{-
  All top-level solutions
  =============================
-}

euler1 :: Int
euler1 = sum . filter isMultiple $ [1..999]
  where isMultiple n = n `isMultOf` 3 || n `isMultOf` 5

euler2 :: Int
euler2 = sum . takeWhile (<4000000) . filter even $ fibs

euler3 :: Integer
euler3 = maximum . primeFactorization $ 600851475143 

euler4 :: Int
euler4 = maximum . filter isPalindrome $ allCombos 100 999
  where allCombos min max = [ x * y | x <- [min..max], y <- [x..max] ]

euler5 :: Int
euler5 = product . map logMagic . takeWhile (<target) $ primes
  where 
    logMagic n = (n^) . floor $ logBase (f n) (f target)
    target = 20
    f = fromIntegral

euler6 :: Int
euler6 =  squareOfSum - sumOfSquare  
  where
    squareOfSum = (^2) . sum $ range
    sumOfSquare = sum . map (^2) $ range
    range = [1..100]

euler7 :: Int
euler7 = last . take 10001 $ primes

euler8 :: Int
euler8 = maximum . prodsList $ euler8String
  where 
    prodsList list
      | length list >= 5 = buildProduct list : prodsList (tail list)
      | otherwise = []
    buildProduct = product . map toInt . take 5  
    toInt n = read [n] :: Int

euler9 :: Int
euler9 = mult . head . filter isMatch $ combos
  where
    combos = [ (a, b, target - a - b) | 
      a <- [1..(target `div` 3)], 
      b <- [(a + 1)..(target `div` 2)] ]
    isMatch (a,b,c) = (a*a + b*b == c*c) && (a + b + c == target)
    mult (a,b,c) = a * b * c
    target = 1000

euler10 :: Integer
euler10 = sum . takeWhile (<2000000) $ primes

euler11 :: Integer
euler11 = undefined

euler12 :: Integer
euler12 = head . filter ((>500) . numOfFactors) $ triangleNumbers
  where
    triangleNumbers = roll 0 1
    roll acc count = acc : roll (acc + count) (count + 1)


{-
  Utilities
  =========
-}

isMultOf :: (Integral a) => a -> a -> Bool
a `isMultOf` b = a `mod` b == 0

fibs :: (Integral a) => [a]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

isPalindrome :: (Show a, Eq a) => a -> Bool
isPalindrome n = show n == (reverse . show $ n)

numOfFactors :: (Integral a) => a -> Int
numOfFactors 1 = 1
numOfFactors n = foldr step 2 [2..root]
  where
    step l acc 
      | l * l == n     = acc + 1
      | n `isMultOf` l = acc + 2
      | otherwise      = acc
    root = floor . sqrt . fromIntegral $ n

-- function not yet used
factors :: (Integral a) => a -> [a]
factors n = [d | d <- [1..n], n `isMultOf` d]


{-
  My prime algorithms
  -------------------
-}

primeFactorization :: (Integral a) => a -> [a]
primeFactorization n 
  | f == n    = [f]
  | otherwise = f : primeFactorization (n `div` f)
  where f = smallestPrimeFactor n

smallestPrimeFactor :: (Integral a) => a -> a
smallestPrimeFactor n = head $ filter (n `isMultOf`) range
  where 
    range = [2..root] ++ [n]
    root = ceiling . sqrt . fromIntegral $ n


{-
  Others' prime algorithms
  ------------------------
-}

-- source: http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)

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
 

{-
  Raw data
  ========
-}

euler8String = 
     "73167176531330624919225119674426574742355349194934"
  ++ "96983520312774506326239578318016984801869478851843"
  ++ "85861560789112949495459501737958331952853208805511"
  ++ "12540698747158523863050715693290963295227443043557"
  ++ "66896648950445244523161731856403098711121722383113"
  ++ "62229893423380308135336276614282806444486645238749"
  ++ "30358907296290491560440772390713810515859307960866"
  ++ "70172427121883998797908792274921901699720888093776"
  ++ "65727333001053367881220235421809751254540594752243"
  ++ "52584907711670556013604839586446706324415722155397"
  ++ "53697817977846174064955149290862569321978468622482"
  ++ "83972241375657056057490261407972968652414535100474"
  ++ "82166370484403199890008895243450658541227588666881"
  ++ "16427171479924442928230863465674813919123162824586"
  ++ "17866458359124566529476545682848912883142607690042"
  ++ "24219022671055626321111109370544217506941658960408"
  ++ "07198403850962455444362981230987879927244284909188"
  ++ "84580156166097919133875499200524063689912560717606"
  ++ "05886116467109405077541002256983155200055935729725"
  ++ "71636269561882670428252483600823257530420752963450"
