module Main where

import Control.Exception (assert)

main :: IO ()
main = do
    [w] <- fmap words getLine
    let n = read w
    print $ fibFast n

testSolution :: IO ()
testSolution = and tests `seq` return ()
    where
        tests = [assert (fibFast 3 == 2) True, assert (fibFast 10 == 55) True] 
                ++ map (\i -> assert (fibFast i == fibNaive i) True) [0..20]

fibFast :: Int -> Int
fibFast = (!!) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Naive solution for comparison
fibNaive :: Int -> Int
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)