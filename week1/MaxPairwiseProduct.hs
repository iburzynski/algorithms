main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    nums <- getLine
    let nums' = [read x :: Int | x <- words nums]
    print $ maxPairwiseProductFast nums'

maxPairwiseProductFast :: (Num a, Ord a) => [a] -> a
maxPairwiseProductFast []  = 0
maxPairwiseProductFast [x] = 0
maxPairwiseProductFast xs  = snd (max1 zipped (0, 0)) * snd (max2 zipped (0, 0))
    where
        zipped          = zip [0..] xs
        max1 [] acc     = acc
        max1 (x:xs) acc = if snd x > snd acc
                          then max1 xs x
                          else max1 xs acc

        max2 [] acc     = acc
        max2 (x:xs) acc = if snd x > snd acc 
                          && fst x /= fst (max1 zipped (0, 0))
                          then max2 xs x
                          else max2 xs acc

maxPairwiseProduct :: (Num a, Ord a) => [a] -> a
maxPairwiseProduct []  = 0
maxPairwiseProduct [x] = 0
maxPairwiseProduct xs  = go xs 0
    where
        go [] acc         = acc
        go (x:xs) acc     = if go' (x:xs) 0 > acc
                            then go xs (go' (x:xs) 0)
                            else go xs acc

        go' [] acc        = acc
        go' [x] acc       = acc
        go' (x:x':xs) acc = if x * x' > acc
                            then go' (x:xs) (x * x')
                            else go' (x:xs) acc