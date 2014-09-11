module Multiples where

-- | Find sum of all multiples of 3 and 5 below n
--
-- >>> multiples 3
-- 0
--
-- >>> multiples 5
-- 3
--
-- >>> multiples 6
-- 8
--
-- [1, 2, 3, 4, 5, 6, 7] -> [3, 5, 6] -> 14
-- >>> multiples 7
-- 14
--
-- >>> multiples 10
-- 23
multiples :: Int -> Int
multiples n = sum [x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]

