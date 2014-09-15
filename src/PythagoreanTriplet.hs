module PythagoreanTriplet where

-- | Special triplet where a + b + c = 1000
--
specialTriplet :: (Integer, Integer, Integer)
specialTriplet = head $ dropWhile (\(a,b,c) -> a + b + c /= 1000) $ [(a,b,c) | c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

