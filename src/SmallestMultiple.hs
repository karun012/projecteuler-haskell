module SmallestMultiple where

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Control.Applicative

-- | Hypothesis
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
smallestEvenNumberEvenlyDivisibleBy xs = head $ dropWhile (not . divisibleByAll (subset xs)) (candidates (subset xs))

-- | Finds the probable candidate range for finding the smallest even number evenly divisible by all the numbers in the range
--
-- >>> candidates [1..2]
-- [2]
--
-- >>> candidates [1..3]
-- [6]
--
-- >>> let xs = [1..10]
-- >>> let max = foldr (*) 1 xs
-- >>> let actual = candidates [1..10]
-- >>> let expected = [10*9*8..max]
-- >>> expected == actual
-- True
--
candidates :: [Integer] -> [Integer]
candidates xs = [min..max]
                    where max = product xs
                          min = (*1) $ round $ (sqrt . fromIntegral) max

ans = 232792560
-- | Tells if a number is divisible by all the numbers in the given set
--
-- >>> divisibleByAll [1,2] 4
-- True
--
-- >>> divisibleByAll [1,4] 3
-- False
--
divisibleByAll :: [Integer] -> Integer -> Bool
divisibleByAll xs n = not $ any ((/=0) . mod n) xs
