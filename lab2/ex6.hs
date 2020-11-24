

sumSquares :: Num a => [a] -> a
sumSquares []  = 0 
sumSquares (x : xs)  = x^2 +  sumSquares xs 







fib :: (Eq a, Num a) => a -> a
fib n = 
    if n == 0 || n == 1 then n
    else fib(n - 1) + fib(n - 2)

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a 
prod' [] = 1
prod' (x:xs) = x * (prod' xs)

length' :: [a] -> Int 
length' [] = 0
length' (x:xs) = 1 + (length' xs)

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

and' :: [Bool] -> Bool
and' [] = False
and' [x] = x
and' (x:xs) = x && and' xs

elem' :: Eq a => a -> [a] -> Bool
elem' a [x] = a == x
elem' a (x:xs) = a == x || elem' a xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs)  = x * 2 : doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs)  = x ^ 2 : squareAll xs

selectEven :: Integral t => [t] -> [t]
selectEven [] = []
selectEven (x:xs) = if x `mod` 2 == 0 then x : selectEven xs
                    else selectEven xs


sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs


prod'2 :: Num a => [a] -> a 
prod'2 = loop 1
    where   loop acc [] = acc
            loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 = loop 0
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc + 1) xs

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<= x) xs 
   rightPart xs = filter (> x) xs

fst2Div :: Integral a => [a] -> Bool
fst2Div (x : y : _) | y `mod` x == 0 = True
                    | y `mod` x /= 0 = False
fst2Div _                            = False