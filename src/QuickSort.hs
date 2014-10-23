module QuickSort where

import Control.Parallel
import Control.Parallel.Strategies
import System.Random
import Criterion.Main

cutoff :: Int
cutoff = 5000

sampleSize :: Int
sampleSize = 1000000

sample :: [Int]
sample = randomList (1 :: Int, sampleSize :: Int) sampleSize g
            where g = mkStdGen 123456

main :: IO ()
main = do
     let sorted = parallelQuickSort sample
     let lastFewIndices = [sampleSize - 1, sampleSize - 2, sampleSize - 3, sampleSize - 4]
     let lastFew = map (\index -> sorted !! index) lastFewIndices
     print lastFew
-- This is used for bencharmarking with Criterion
--main = defaultMain [
--            bench "parallel" $ nf parallelQuickSort sample,
--            bench "serial" $ nf serialQuickSort sample
--       ]

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = take n . randomRs bnds

parallelQuickSort :: Ord a => [a] -> [a]
parallelQuickSort [] = []
parallelQuickSort xs | length xs < cutoff = serialQuickSort xs
parallelQuickSort (x:xs) = lows `par` highs `pseq` lows ++ [x] ++ highs 
                      where lows = parallelQuickSort [a | a <- xs, a <= x]
                            highs = parallelQuickSort [b | b <- xs, b > x]

serialQuickSort :: Ord a => [a] -> [a]
serialQuickSort [] = []
serialQuickSort (x:xs) = lows ++ [x] ++ highs 
                      where lows = serialQuickSort [a | a <- xs, a <= x]
                            highs = serialQuickSort [b | b <- xs, b > x]
