module Main where

main :: IO ()
main = do
    [n] <- fmap words getLine
    print $ fibLastDigit (read n)

fibLastDigit :: Int -> Int
fibLastDigit = (!!) ldfibs
    where sumMod10 x y = ((x `rem` 10) + (y `rem` 10)) `rem` 10
          ldfibs = 0 : 1 : zipWith sumMod10 ldfibs (tail ldfibs)