-- Finds the maximum sum of products from possible pairings of two input lists
module Main where

import Data.List (sortBy)

main :: IO ()
main = do
    _  <- getLine
    as <- fmap (map read . words) getLine
    bs <- fmap (map read . words) getLine
    print $ maxDotProduct as bs

maxDotProduct :: [Int] -> [Int] -> Integer
maxDotProduct as bs = sum $ zipWith (*) (process as) (process bs)
    where
     -- Sort list in descending order and convert to integers 
        process = (fmap toEnum :: [Int] -> [Integer]) . sortBy (flip compare)