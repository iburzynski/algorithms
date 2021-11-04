module Main where

main :: IO ()
main = do
    [from, to] <- fmap words getLine
    print $ fibPartialSum (read from) (read to)

fibPartialSum :: Int -> Int -> Int
fibPartialSum x y = (maxSum - minSum) `mod` 10 
    where maxSum = fibSumLastDigit (max x y) 
          minSum = fibSumLastDigit (min x y - 1)

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
fibPartialSumNaive :: Integer -> Integer -> Int
fibPartialSumNaive from to = 
    let (a', b', _) = helper (0, 1, 0) from
        (_, _, s)   = helper (a', b', a') (to - from)
    in s
    where
        helper (a, b, s) 0 = (a, b, s `mod` 10)
        helper (a, b, s) i = helper (b, a + b, s + b) (i - 1)