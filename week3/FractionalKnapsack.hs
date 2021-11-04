-- Maximizes the total value of items that fit in a knapsack, allowing fractions 
module Main where

import Data.List     (sortBy)
import Control.Monad (replicateM)
import Numeric       (showFFloat)

type Value = Int
type Weight = Int

main :: IO ()
main = do
    [n', capacity'] <- fmap words getLine
    let (n, capacity) = (read n', read capacity')
    items <- replicateM n $ fmap ((\[v, w] -> (read v, read w)) . words) getLine
    printFloat $ getOptimalValue capacity items
        where printFloat x = putStrLn $ showFFloat (Just 4) x ""

getOptimalValue :: Int -> [(Value, Weight)] -> Double
getOptimalValue capacity = go (fromIntegral capacity) 0 . valSort
    where
     -- Sort items in descending order by value/weight ratio
        valSort            = sortBy (flip (\(v, w) (v', w') ->
                                     compare (v / w) (v' / w'))) . fmap dbl
        dbl (a, b)         = (fromIntegral a, fromIntegral b)

        go _ t []          = t
        go c t ((v, w):xs) = if w <= c
                          -- Add full weight/value if possible
                             then go (c - w) (t + v) xs
                          -- Else add fractional weight/value up to capacity
                             else t + (v * (c / w))