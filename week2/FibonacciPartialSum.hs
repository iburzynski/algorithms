module Main where

main :: IO ()
main = do
  [from, to] <- fmap words getLine
  print $ fibonacciPartialSum (read from) (read to)

fibonacciPartialSum :: Int -> Int -> Int
fibonacciPartialSum x y = (maxSum - minSum) `mod` 10 
    where maxSum = fibonacciSumLastDigit (max x y) 
          minSum = fibonacciSumLastDigit (min x y - 1)

fibonacciSumLastDigit :: Int -> Int
fibonacciSumLastDigit x = (fromIntegral (fibonacciHuge (x + 2) 10) - 1) `mod` 10

fibonacciHuge :: Int -> Int -> Integer
fibonacciHuge n m = rem (fibonacci $ n `rem` pisano m) (toEnum m)

pisano :: Int -> Int
pisano m = if m < 2 then 0
           else go m 0 1 0
    where go m a b acc = if a == 0 && b == 1 && acc /= 0 then acc
                         else go m b ((a + b) `rem` m) (acc + 1)

fibonacci :: Int -> Integer
fibonacci = (!!) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Naive solution for comparison
fibonacciPartialSumNaive :: Integer -> Integer -> Int
fibonacciPartialSumNaive from to = 
    let (a', b', _) = helper (0, 1, 0) from
        (_, _, s)   = helper (a', b', a') (to - from)
    in s
    where
        helper (a, b, s) 0 = (a, b, s `mod` 10)
        helper (a, b, s) i = helper (b, a + b, s + b) (i - 1)