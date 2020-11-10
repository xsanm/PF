--Autor: Kamil Kurowski

--1.7.1
add2C :: Num a => (a -> (a -> a))
add2C x y = x + y
--prawostronnie łączny

--1.7.2
add3T :: Num a => (a, a, a) -> a
add3T (a, b, c) = a + b + c

add3C :: Num a => (a -> (a -> (a -> a)))
add3C a b c = a + b + c

--1.7.3
--((add3C 1) 2) 3

--1.7.4
curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f a b = f (a, b)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a, b) = f a b

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

--2.2.1
fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = (subtract 5)

--2.2.2
flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f a b   =  f b a

--4.2.1
isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

--4.2.2 (zakladam numeracje od 1)
getElemAtIdx idx tmp = head (drop (idx - 1) tmp)


--5.2.1
 [(a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100], a ^ 2 + b ^ 2 == c ^ 2]

 --5.2.2
 --Nie jest poprawna - dla 1 zwraca true, brakuje warunku: n >= 2;
 --nie jest efektywna, wystarczy sprawdzac do sqrt(n) oraz np odrazu wyeliminować parzyste n

 --5.2.3

--6.7.1
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

--7.3.1
prod'2 :: Num a => [a] -> a 
prod'2 = loop 1
    where   loop acc [] = acc
            loop acc (x:xs) = loop (x * acc) xs

length'2 :: [a] -> Int
length'2 = loop 0
  where loop acc [] = acc
        loop acc (x:xs) = loop (acc + 1) xs

--9.2.1
qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<= x) xs 
   rightPart xs = filter (> x) xs

--10.2.1
fst2Div :: Integral a => [a] -> Bool
fst2Div (x : y : _) | y `mod` x == 0 = True
                    | y `mod` x /= 0 = False
fst2Div _                            = False



 