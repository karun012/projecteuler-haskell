module PythagoreanTriplet where

import Data.List

-- | Generating triplets with the required conditions using Euclid's formula
--
-- Given two integers m and n with m > n
-- A pythagorean triplet (a,b,c) can be defined in terms of m and n as 
-- a = m^2 - n^2, b = 2 * m * n, c = m^2 + n^2
--
specialTriplet :: Maybe (Integer,Integer,Integer)
specialTriplet = find (\(a,b,c) -> a + b + c == 1000) (triplets combinations)

triplets :: [(Integer,Integer)] -> [(Integer,Integer,Integer)]
triplets mnCombinations = map (\(m,n) -> (m^2 - n^2, 2 * m * n, m^2 + n^2)) mnCombinations

combinations :: [(Integer,Integer)]
combinations = [(m,n) | m <- [1..], n <- [1..m]]
