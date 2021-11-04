-- Represents a pos. integer as sum of as many unique pos. integers as possible
module Main where

main :: IO ()
main = do
    n  <- getLine
    let summands = optimalSummands (read n)
    print $ length summands
    putStrLn $ unwords (map show summands)

optimalSummands :: Int -> [Int]
optimalSummands = go [] 1
    where
        go ss _ 0 = reverse ss
        go ss i r = if i + (i + 1) > r
                    -- If current plus next increment exceeds remaining supply,
                    -- append all of the remaining supply as the final summand
                    then reverse $ r : ss
                    -- else append current increment, adjust values and proceed
                    else go (i : ss) (i + 1) (r - i) 