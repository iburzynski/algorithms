module Main where

main :: IO ()
main = do
  [a, b] <- fmap words getLine
  print $ lcmFast (read a) (read b)

lcmFast :: Integral a => a -> a -> a
lcmFast 0 _ = 0
lcmFast _ 0 = 0
lcmFast x y = abs(x * y `quot` gcd x y)

-- Naive solution for comparison
lcmNaive :: Int -> Int -> Integer
lcmNaive a b = 
    minimum [ l | l <- [1 .. a' * b'], l `mod` a' == 0 && l `mod` b' == 0 ]
  where
    a' = fromIntegral a :: Integer
    b' = fromIntegral b :: Integer

