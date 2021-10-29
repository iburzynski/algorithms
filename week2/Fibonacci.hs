module Main where

import Control.Exception (assert)

main :: IO ()
main = do
  [w] <- fmap words getLine
  let n = read w
  print $ fibonacciFast n

testSolution :: IO ()
testSolution = and tests `seq` return ()
  where
    tests =
      [ assert (fibonacciFast 3  == 2)  True
      , assert (fibonacciFast 10 == 55) True
      ] ++ map (\i -> assert (fibonacciFast i == fibonacciNaive i) True) [0..20]

fibonacciFast :: Int -> Int
fibonacciFast = (!!) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Naive solution for comparison
fibonacciNaive :: Int -> Int
fibonacciNaive 0 = 0
fibonacciNaive 1 = 1
fibonacciNaive n = fibonacciNaive (n - 1) + fibonacciNaive (n - 2)