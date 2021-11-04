module Main where

main :: IO ()
main = do
    [n, m] <- fmap words getLine
    print $ fibHugeFast (read n) (read m)

fibHugeFast :: Int -> Int -> Integer
fibHugeFast n m = rem (fibFast $ n `rem` pisano m) (toEnum m)

pisano :: Int -> Int
pisano m = if m < 2 then 0 else go m 0 1 0
    where go m a b acc = if a == 0 && b == 1 && acc /= 0 then acc
                         else go m b ((a + b) `rem` m) (acc + 1)

fibFast :: Int -> Integer
fibFast = (!!) fibs
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Naive solution for comparison
fibHugeNaive :: Int -> Int -> Int
fibHugeNaive n m = helper (0, 1) n
    where
        helper (a, _) 0 = a `mod` m
        helper (a, b) i = helper (b, a + b) (i - 1)
