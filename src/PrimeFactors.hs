module Main where

import System.Environment
import Control.Applicative 

main :: IO ()
main = do
    args <- getArgs
    let firstArg = getFirstArg args
    let primeFactor = largestPrimeFactorV1 firstArg
    putStrLn (show primeFactor)

toInt n = read n :: Integer

getFirstArg :: [String] -> Integer
getFirstArg args = head $ map (toInt) args

squareRootRounded :: Integer -> Integer
squareRootRounded = round . sqrt . fromIntegral

factors :: Integer -> [Integer]
factors n = filter ((==0) . mod n) [squareRootRounded n,(squareRootRounded n)-1..1]

candidates :: Integer -> [Integer]
candidates n = [3,5..squareRootRounded n]
--candidates n = [2..n-1]

isPrime :: Integer -> Bool
isPrime n = all ((/=0) . rem n) (candidates n)

isNotPrime :: Integer -> Bool
isNotPrime n = any ((==0) . rem n) (candidates n)

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor = head . dropWhile (isNotPrime) . factors

largestPrimeFactorV1 :: Integer -> Integer
largestPrimeFactorV1 = head . dropWhile (not . isPrime) . factors
