-- Compile - ghc -main-is CollatzSequence -threaded -O2 
-- Run - CollaztSequence -N -s
module CollatzSequence where

import Data.List
import Data.List.Split
import Control.Concurrent.ParallelIO.Global

main :: IO ()
main = do
      let blockSize = 100000
      let min = 1
      let max = 1000000
      let blocks = chunksOf blockSize [min..max]
      results <- parallel (map largestCollatzSequenceInBlock blocks)
      let maxLengthCollatzSequence = maximumBy (\(_, x) (_, y) -> if x > y then GT else LT) results
      print $ "Max length collatz sequence between " ++ show min ++ " and " ++ show max ++ ": " ++ show maxLengthCollatzSequence
      stopGlobalPool

largestCollatzSequenceInBlock :: [Integer] -> IO (Integer,Integer)
largestCollatzSequenceInBlock xs = do
                                let start = head xs
                                let chains = collatzChains xs
                                let largestCollatzSequence = (head . sortCollatzSequences) chains
                                print $ "Largest sequence in block starting at " ++ show start ++ " : " ++ show (largestCollatzSequence) ++ "\n"
                                return (largestCollatzSequence)

collatz n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

collatzChain :: Integer -> [Integer]
collatzChain 1 = [1]
collatzChain n = collatz n : collatzChain (collatz n)

sortCollatzSequences :: [(Integer, Integer)] -> [(Integer, Integer)]
sortCollatzSequences xs = sortBy (\(_,x) (_,y) -> if x < y then GT else LT) xs

collatzChains :: [Integer] -> [(Integer, Integer)]
collatzChains xs = map (\x -> (x, (toInteger . (+1) . length . takeWhile (/=1) . iterate collatz) x)) xs
-- collatzChains xs = map (\x -> (x, (toInteger . length . takeWhile (/=1) . collatzChain) x)) xs

