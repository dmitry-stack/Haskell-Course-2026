{-# LANGUAGE BangPatterns #-}

module Homework where

-- Task 1: Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [(p, q) | p <- ps, q <- ps, p <= q, p + q == n]
  where
    ps = primesTo n

-- Task 2: Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  [ (x,y)
  | x <- xs
  , y <- xs
  , x < y
  , gcd x y == 1
  ]

-- Task 3: Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | otherwise = n `elem` primesTo n

-- Task 4: Matrix Multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = [[sum [a !! i !! k * b !! k !! j | k <- [0..p-1]]
               | j <- [0..n-1]]
               | i <- [0..m-1]]
  where
    m = length a
    p = length (head a)
    n = length (head b)

-- Helper: remove first occurrence
removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne y (x:xs)
  | y == x    = xs
  | otherwise = x : removeOne y xs

-- Task 5: Permutations
permutations :: Eq a => Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations k xs = [x : ys | x <- xs, ys <- permutations (k-1) (removeOne x xs)]

-- Task 6: Hamming Numbers
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys
merge xs [] = xs
merge [] ys = ys

hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))

-- Task 7: Integer Power with Bang Patterns
power :: Int -> Int -> Int
power b e = go b e 1
  where
    go _ 0 !acc = acc
    go base exp !acc = go base (exp - 1) (base * acc)

-- Task 8: Running Maximum
listMaxSeq :: [Int] -> Int
listMaxSeq (x:xs) = go xs x
  where
    go [] acc = acc
    go (y:ys) acc = let nextAcc = max y acc
                    in nextAcc `seq` go ys nextAcc

listMaxBang :: [Int] -> Int
listMaxBang (x:xs) = go xs x
  where
    go [] !acc = acc
    go (y:ys) !acc = go ys (max y acc)

-- Task 9: Infinite Prime Stream
primes :: [Int]
primes = sieve [2..]

isPrimeInfinite :: Int -> Bool
isPrimeInfinite n
  | n < 2     = False
  | otherwise = n == head (dropWhile (< n) primes)

-- Task 10

mean :: [Double] -> Double
mean xs = go xs 0 0
  where
    go [] s n
      | n == 0    = 0
      | otherwise = s / n
    go (x:xt) s n = go xt (s + x) (n + 1)


meanStrict :: [Double] -> Double
meanStrict xs = go xs 0 0
  where
    go [] !s !n
      | n == 0    = 0
      | otherwise = s / n
    go (x:xt) !s !n = go xt (s + x) (n + 1)


stats :: [Double] -> (Double, Double)
stats xs = go xs 0 0 0
  where
    go [] !s !s2 !n
      | n == 0    = (0, 0)
      | otherwise =
          let mean = s / n
              var  = (s2 / n) - mean * mean
          in (mean, var)

    go (x:xt) !s !s2 !n =
      go xt (s + x) (s2 + x*x) (n + 1)

main :: IO ()
main = do
    putStrLn "Task 1: Goldbach Pairs"
    print (goldbachPairs 28)

    putStrLn "\nTask 2: Coprime Pairs"
    print (coprimePairs [8,9,21,25])

    putStrLn "\nTask 3: Sieve / Primes"
    print (primesTo 30)

    putStrLn "\nTask 4: Matrix Multiplication"
    let a = [[1,2,3],[4,5,6]]
    let b = [[7,8],[9,10],[11,12]]
    print (matMul a b)

    putStrLn "\nTask 5: Permutations"
    print (permutations 2 [1,2,3])

    putStrLn "\nTask 6: Hamming Numbers"
    print (take 10 hamming)

    putStrLn "\nTask 7: Power"
    print (power 3 5)

    putStrLn "\nTask 8: List Maximum"
    print (listMaxBang [3,1,9,2,7])

    putStrLn "\nTask 9: Infinite Primes"
    print (take 10 primes)

    putStrLn "\nTask 10: Mean & Variance"
    print (stats [1,2,3,4,5])
