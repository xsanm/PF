sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x ^ 2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum2     = sumWith (+ 0)
sumSqr2  = sumWith (^2)
sumCube2 = sumWith (^3)
sumAbs2 :: [Integer] -> Integer
sumAbs2  = sumWith abs

listLength = sumWith (const 1)
listLength2 = sumWith (\x -> 1)

prod' :: Num a => [a] -> a
prod' [] = 0
prod' [x] = x
prod' (x:xs) = x * prod' xs
