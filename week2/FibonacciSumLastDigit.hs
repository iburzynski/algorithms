module Main where

main :: IO ()
main = do
    [n] <- fmap words getLine
    print $ fibSumLastDigit (read n)

fibSumLastDigit :: Int -> Int
fibSumLastDigit x = (fromIntegral (fibHuge (x + 2) 10) - 1) `mod` 10

fibHuge :: Int -> Int -> Integer
fibHuge n m = rem (fib $ n `rem` pisano m) (toEnum m)

pisano :: Int -> Int
pisano m = if m < 2 then 0 else go m 0 1 0
    where go m a b acc = if a == 0 && b == 1 && acc /= 0 then acc
                         else go m b ((a + b) `rem` m) (acc + 1)

fib :: Int -> Integer
fib = (!!) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Naive solution for comparison
fibSumNaive :: Int -> Int
fibSumNaive = helper (0, 1, 0)
    where
        helper (_, _, s) 0 = s `mod` 10
        helper (a, b, s) i = helper (b, a + b, s + b) (i - 1)