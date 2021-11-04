-- Finds the smallest set of points to intersect every line segment in a list
module Main where

import Control.Monad (replicateM)
import Data.List     (sort, minimumBy)

main :: IO ()
main = do
    n <- read <$> getLine
    segs <- replicateM n $ fmap ((\[s, e] -> (read s, read e)) . words) getLine
    let points = optimalPoints segs
    print $ length points
    putStrLn $ unwords (map show points)

optimalPoints :: [(Int, Int)] -> [Int]
optimalPoints = go [] . sort
    where
        go pts []          = reverse pts
        go pts ((_, r):ss) = let xs = filter (\(_, r') -> r' < r) ss in 
                          -- Check for any segments that end before this one
         -- If none, add pt & repeat w/ only segs starting after this one's end
            if null xs then go (r : pts) (filter (\(l, _) -> l > r) ss)
         -- Else set boundary to the end of the shortest segment and continue
            else let bnd = snd $ minimumBy (\(_, r) (_, r') -> compare r r') xs
                 in go (bnd : pts) (filter (\(l, _) -> l > bnd) ss)