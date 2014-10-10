module HighlyDivisibleTriangleNumber where


triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

factors :: Integer -> [Integer]
factors n = fs ++ (reverse $ map (div n) fs)
           where fs = filter((==0) . mod n) [1..round . sqrt $ fromIntegral n]

triangleAndFactors :: Integer -> (Integer, Integer)
triangleAndFactors n = (t, numberOfFactors) 
                          where t = triangle n
                                numberOfFactors = toInteger $ (length . factors) t

tf :: Integer -> (Integer, [Integer])
tf n = (t, f) 
          where t = triangle n
                f= factors t 

infiniteTrianglesAndFactors :: Integer -> [(Integer, Integer)]
infiniteTrianglesAndFactors n = map triangleAndFactors [1..]

triangleNumberWithFactorsGreaterThan :: Integer -> (Integer, Integer)
triangleNumberWithFactorsGreaterThan n = head $ dropWhile (\(t, fs) -> fs < n) (infiniteTrianglesAndFactors n)
