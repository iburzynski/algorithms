-- Finds the min. number of fuel stops needed to travel a given distance
module Main where

main :: IO ()
main = do
    d <- (read :: String -> Int) <$> getLine
    m <- (read :: String -> Int) <$> getLine
    _ <- getLine
    stops <- (fmap read :: [String] -> [Int]) . words <$> getLine
    print $ minRefills d m stops

minRefills :: Int -> Int -> [Int] -> Int
minRefills d m = go 0 . (0:) -- Prepend zero (origin) to the list of stops 
    where
        go r []         = r
        go r (s:ss)     = if d - s > m
                       -- If can't reach destination from current position
                          then let ss' = go' s (s:ss)
                       -- Reach as many stops as possible with available fuel
                               in if ss' /= (s:ss)
                               -- If at least one more stop has been reached,
                               -- refuel and continue with remaining stops
                                  then go (r + 1) ss'
                               -- Else return impossible
                                  else (-1)
                          else r
        go' _ []        = []
        go' _ [s]       = [s]
        go' c (s:s':ss) = if s' - c <= m
                       -- If dist. to next stop doesn't exceed fuel economy
                       -- advance to next stop. Else return all remaining stops
                          then go' c (s':ss)
                          else s:s':ss