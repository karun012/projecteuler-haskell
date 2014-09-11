module SmallestMultiple where

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative

-- | Hypothesis 1
--
-- If x is evenly divisible by n, then x is evenly divisible by all the factors of n
--
-- prop> \(Positive x) (Positive n) -> (x `mod` n == 0) == (all (\y -> x `mod` y == 0) (n : factors n))
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
-- smallestEvenNumberEvenlyDivisibleBy [1..20]
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
divisibleByAll xs n = failFastAll ((==0) . mod n) xs

-- | Faster version of any
--
-- >>> failFastAll (even) [2,4,5,6]
-- False
--
failFastAll :: (a -> Bool) -> [a] -> Bool
failFastAll fn xs = (length xs) == (length . takeWhile fn) xs
