module Main where

main :: IO ()
main = do
  [n] <- fmap words getLine
  print $ fibonacciSumSquares (read n)

fibonacciSumSquares :: Int -> Int
fibonacciSumSquares x = fromIntegral $ (n * nPlusOne) `rem` 10
    where n        = fibonacciHuge x 10
          nPlusOne = fibonacciHuge (x + 1) 10

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