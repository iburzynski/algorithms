module Main where

main :: IO ()
main = do
    [n] <- fmap words getLine
    print $ fibSumSquares (read n)

fibSumSquares :: Int -> Int
fibSumSquares x = fromIntegral $ (n * nPlusOne) `rem` 10
    where n        = fibHuge x 10
          nPlusOne = fibHuge (x + 1) 10

fibHuge :: Int -> Int -> Integer
fibHuge n m = rem (fib $ n `rem` pisano m) (toEnum m)

pisano :: Int -> Int
pisano m = if m < 2 then 0 else go m 0 1 0
    where go m a b acc = if a == 0 && b == 1 && acc /= 0 then acc
                         else go m b ((a + b) `rem` m) (acc + 1)

fib :: Int -> Integer
fib = (!!) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)