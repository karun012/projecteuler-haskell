module SmallestMultiple where

-- | Hypothesis
--
-- If x is evenly divisible by n, then x is evenly divisible by all the factors of n
--
-- >>> let x = 16
-- >>> let n = 8
-- >>> (x `mod` n == 0) == (all (\y -> x `mod` y == 0) (factors n))
-- True
--
-- >>> let x = 1200
-- >>> let n = 120
-- >>> (x `mod` n == 0) == (all (\y -> x `mod` y == 0) (factors n))
-- True
--
factors :: Integer -> [Integer]
factors n = filter ((==0) . mod n) [1..n - 1]

-- | The subset of numbers needed to prove that a number is divisible by all numbers in a set
--
-- >>> let set = [1..10]
-- >>> subset set
-- [6,7,8,9,10]
--
subset :: [Integer] -> [Integer]
subset set = filter (`notElem` (concatMap factors set)) set

-- | Smallest number that is evenly divisible by all numbers in a range
--
-- >>> smallestEvenNumberEvenlyDivisibleBy [1,2]
-- 2
--
-- >>> smallestEvenNumberEvenlyDivisibleBy [1..10]
-- 2520
--
-- >>> smallestEvenNumberEvenlyDivisibleBy [1..20]
-- 2520
--
smallestEvenNumberEvenlyDivisibleBy :: [Integer] -> Integer
smallestEvenNumberEvenlyDivisibleBy xs = head $ dropWhile (not . divisibleByAll (subset xs)) [(last xs),(last xs + 2)..]

-- | Tells if a number is divisible by all the numbers in the given set
--
-- >>> divisibleByAll [1,2] 4
-- True
--
-- >>> divisibleByAll [1,4] 3
-- False
--
divisibleByAll :: [Integer] -> Integer -> Bool
divisibleByAll xs n = any ((/=0) . mod n) xs
