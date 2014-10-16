module ParallelFibonacci where

import Control.Parallel
import Criterion.Main
import Control.Applicative

parallelismCutoff :: Int
parallelismCutoff = 5

main :: IO ()
main = defaultMain [
    bgroup "parallelFib" [ bench "15" $ whnf parFib 15,
                           bench "45" $ whnf parFib 45
                         ],
    bgroup "serial" [ bench "15" $ whnf fib 15,
                      bench "45" $ whnf fib 45
                    ]
    ]

parFib :: Int -> Int
parFib n | n < parallelismCutoff = fib n
parFib n = p `par` q `pseq` (p + q)
              where
                 p = parFib $ n - 2
                 q = parFib $ n - 1

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 2) + fib (n - 1)
