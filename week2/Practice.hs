module Practice where

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

fibNum :: Int -> Integer
fibNum = (!!) fib

lastDigitFib :: [Int]
lastDigitFib = 0 : 1 : zipWith (\x y -> ((x `rem` 10) + (y `rem` 10)) `rem` 10) 
                               lastDigitFib (tail lastDigitFib)

lastDigitFibNum :: Int -> Int
lastDigitFibNum = (!!) lastDigitFib

gcd' :: Integral a => a -> a -> a
gcd' x y = go (abs x) (abs y)
    where go a 0 = a
          go a b = go b $ a `rem` b

lcm' :: Integral a => a -> a -> a
lcm' 0 _ = 0
lcm' _ 0 = 0
lcm' x y = abs(x * y `quot` gcd' x y)