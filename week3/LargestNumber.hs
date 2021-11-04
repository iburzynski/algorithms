-- Creates the largest possible number by combining digits from a list
module Main where

import Data.List (sortBy)

main :: IO ()
main = do
    _  <- getLine
    ns <- fmap (map read . words) getLine
    putStrLn $ largestNumber ns

largestNumber :: [Int] -> String
largestNumber = concat . sortBy (flip compareNums) . map show

compareNums :: String -> String -> Ordering
compareNums x y = compare (read $ x ++ y :: Int) (read $ y ++ x :: Int)