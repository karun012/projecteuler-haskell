module Primes where


delete :: Integer -> [Integer]
delete n = [i + j + 2 * i * j | i <- [1..n], j <- [1..i], (i + j + 2 * i * j) < n]

-- | Sieve of sundaram
--
-- >>> take 3 $ sieve 5
-- [2,3,5]
--
-- >>> take 6 $ sieve 6
-- [2,3,5,7,11,13]
--
sieve :: Integer -> [Integer]
sieve n = 2 : [2 * x + 1 | x <- [1..n], x `notElem` delete n]

-- | Get's the nth prime number in a list of prime numbers starting with 2
--
-- >>> nthPrime 1
-- 2
--
-- >>> nthPrime 2
-- 3
--
-- >>> nthPrime 3
-- 5
--
-- >>> nthPrime 4
-- 7
--
-- >>> nthPrime 5
-- 11
--
-- >>> nthPrime 6
-- 13
--
-- >>> nthPrime 7
-- 15
--
-- >>> nthPrime 8
-- 17
--
-- nthPrime 10001
-- 19
--
nthPrime :: Integer -> Integer
nthPrime n = last $ (take . fromIntegral) n $ sieve n

