module PrimeFactors where

squareRootRounded :: Integer -> Integer
squareRootRounded = round . sqrt . fromIntegral

factors :: Integer -> [Integer]
factors n = filter ((==0) . mod n) [squareRootRounded n,(squareRootRounded n)-1..1]

candidates :: Integer -> [Integer]
candidates n = [3,5..squareRootRounded n]
--candidates n = [2..n-1]

isPrime :: Integer -> Bool
isPrime n = all ((/=0) . rem n) (candidates n)

lazyPrimeSequence = error "todo"

-- TODO change to headMay or headOr
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor = head . dropWhile (not . isPrime) . factors
