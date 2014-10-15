-- Compile - ghc -main-is CollatzSequence -threaded -O2 
-- Run - CollaztSequence -N -s
module CollatzSequence where

import Data.List
import Data.List.Split
import Control.Concurrent.ParallelIO.Global
import Control.Concurrent

main :: IO ()
main = do
      maxSimultaneousThreads <- getNumCapabilities
      print $ "Number of threads that can be run in parallel: " ++ show maxSimultaneousThreads
      let min = 1
      let max = 1000000
      let blockSize = max `div` maxSimultaneousThreads
      let blocks = chunksOf (fromIntegral blockSize) [min..max]
      results <- parallel (map largestCollatzSequenceInBlock blocks)
      let maxLengthCollatzSequence = largestChain results
      print $ "Max length collatz sequence between " ++ show min ++ " and " ++ show max ++ ": " ++ show maxLengthCollatzSequence
      stopGlobalPool

largestCollatzSequenceInBlock :: [Int] -> IO (Int,Int)
largestCollatzSequenceInBlock xs = do
                                let start = head xs
                                let chains = collatzChains xs
                                let largestCollatzSequence = largestChain chains
                                print $ "Largest sequence in block starting at " ++ show start ++ " : " ++ show largestCollatzSequence ++ "\n"
                                return (largestCollatzSequence)

largestChain :: [(Int, Int)] -> (Int, Int)
largestChain xs = maximumBy (\(_, x) (_, y) -> if x > y then GT else LT) xs

collatz n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

collatzChains :: [Int] -> [(Int, Int)]
collatzChains xs = map (\x -> (x, (accountForMissingElementFromSequence . length . takeWhile (/=1) . iterate collatz) x)) xs

accountForMissingElementFromSequence :: Int -> Int
accountForMissingElementFromSequence = (+1)
