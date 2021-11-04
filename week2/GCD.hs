module Main where

main :: IO ()
main = do
    [a, b] <- fmap words getLine
    print $ gcdFast (read a) (read b)

gcdFast :: Int -> Int -> Int
gcdFast x y = go (abs x) (abs y)
    where go a 0 = a
          go a b = go b $ a `rem` b

-- Naive solution for comparison
gcdNaive :: Int -> Int -> Int
gcdNaive a b = maximum [d | d <- [1..min a b], a `mod` d == 0 && b `mod` d == 0]