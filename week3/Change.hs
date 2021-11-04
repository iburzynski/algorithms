-- Finds min. number of coins (1, 5, and 10) to make change for input value
module Main where

main :: IO ()
main = do
  [m] <- fmap words getLine
  print $ getChange (read m)

getChange :: Int -> Int
getChange = go 0
    where
        go chg bal
            | bal >= 10 = go (chg + (bal `quot` 10)) (bal `rem` 10) 
            | bal >= 5  = go (chg + (bal `quot` 5)) (bal `rem` 5) 
            | otherwise = chg + bal