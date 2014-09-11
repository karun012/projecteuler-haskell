module Fibonacci where

-- | Sum up even fibonacci numbers up to 4 million
--
-- >>> sumOfEvenFibonacciUpTo 0
-- 0
--
-- >>> sumOfEvenFibonacciUpTo 3
-- 2
--
-- >>> sumOfEvenFibonacciUpTo 10
-- 10
--
-- >>> sumOfEvenFibonacciUpTo 35
-- 44
--
-- Not running this since it takes too long
-- sumOfEvenFibonacciUpTo 4000000
-- 4613732
--
sumOfEvenFibonacciUpTo :: Integer -> Integer
sumOfEvenFibonacciUpTo 0 = 0
sumOfEvenFibonacciUpTo n = sum $ filter even $ takeWhile (< n) (fibonacciUpTo n)

-- | Fibonacci numbers up to n
--
-- >>> fibonacciUpTo 1
-- []
--
-- >>> fibonacciUpTo 2
-- [1,1]
--
-- >>> fibonacciUpTo 3
-- [1,1,2]
--
-- >>> fibonacciUpTo 4
-- [1,1,2,3]
--
fibonacciUpTo :: Integer -> [Integer]
fibonacciUpTo 1 = []
fibonacciUpTo 2 = fib 1 : fib 2 : []
fibonacciUpTo n = fibonacciUpTo (n - 1) ++ [fib n]

-- | Nth fibonacci number
--
-- >>> fib 1
-- 1
--
-- >>> fib 2
-- 1
--
-- >>> fib 3
-- 2
--
-- >>> fib 4
-- 3
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | Lazy infinite fibonacci sequence
--
-- The first two fibonacci numbers are
-- 1 1
--
-- If we take the tail of the sequence, and put it below the sequence
-- 1 1
-- 1
-- ----
-- 2 <- Sum it, and we get the tail of the tail of the fibonacci sequence
--
-- Sequence - 1 1 2
--
-- Taking the tail of that, and adding
-- 1 1 2
-- 1 2
-- ------
-- 2 3
--
-- Sequence - 1 1 2 3
--
-- 1 1 2 3
-- 1 2 3
-- -------
-- 2 3 5
--
-- Sequence - 1 1 2 3 5
--
-- >>> take 0 $ lazyInfiniteFibonacciSequence
-- []
--
-- >>> take 1 $ lazyInfiniteFibonacciSequence
-- [1]
--
-- >>> take 2 $ lazyInfiniteFibonacciSequence
-- [1,1]
--
-- >>> take 3 $ lazyInfiniteFibonacciSequence
-- [1,1,2]
--
lazyInfiniteFibonacciSequence :: [Integer]
lazyInfiniteFibonacciSequence = 1 : 1 : zipWith (+) lazyInfiniteFibonacciSequence (tail lazyInfiniteFibonacciSequence)

-- | Sum of even fibonacci numbers using the lazy infinite fibonacci sequence
--
-- >>> sumOfEvenFibonacciUpTo' 0
-- 0
--
-- >>> sumOfEvenFibonacciUpTo' 3
-- 2
--
-- >>> sumOfEvenFibonacciUpTo' 10
-- 10
--
-- >>> sumOfEvenFibonacciUpTo' 35
-- 44
--
-- >>> sumOfEvenFibonacciUpTo' 4000000
-- 4613732
--
sumOfEvenFibonacciUpTo' :: Integer -> Integer
sumOfEvenFibonacciUpTo' n = (sum . filter even . takeWhile (<n)) lazyInfiniteFibonacciSequence
