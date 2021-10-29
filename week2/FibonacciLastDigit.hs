module Main where

main :: IO ()
main = do
  [n] <- fmap words getLine
  print $ fibonacciLastDigit (read n)

fibonacciLastDigit :: Int -> Int
fibonacciLastDigit = (!!) ldfibs
    where sumMod10 x y = ((x `rem` 10) + (y `rem` 10)) `rem` 10
          ldfibs = 0 : 1 : zipWith sumMod10 ldfibs (tail ldfibs)