--1.2.1
f1 = \x -> x - 2

f2 = \x y -> sqrt(x^2 + y^2)
f2' = \(x, y) -> sqrt(x^2 + y^2)

--f3 :: Int -> Int -> Int -> Double
f3 = \x y z -> sqrt(fromIntegral x^2 + fromIntegral y^2 + fromIntegral z^2)
f3' = \(x, y, z) -> sqrt(fromIntegral x^2 + fromIntegral y^2 + fromIntegral z^2)


--1.2.2
f4 = \x -> 2 * x
f5 = \x -> x * 2
f6 = \x -> 2 ^ x
f7 = \x -> x ^ 2
f8 = \x -> 2 / x
f9 = \x -> x / 3
f10 = \x -> 4 - x

--1.2.3
f11 = \x -> sqrt x
f12 = \x -> abs x
f13 = \x -> log x
f14 = \x -> x
f15 = \x y-> x

--1.2.4
f16 = \x -> if x `mod` 2 == 0 then True else False
f16' = \x ->  x `mod` 2 == 0
f17 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)
f18 = \x -> if x == 1 then 3 else 0


--2.2.1
sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x ^ 2 + sumSqr' xs

--2.2.2
sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

--2.2.3
sum2     = sumWith (+ 0)
sumSqr2  = sumWith (^2)
sumCube2 = sumWith (^3)
sumAbs2  = sumWith abs

--2.2.4
--Wywołać: sumWith (^5) [1..15]

--2.2.5
listLength = sumWith (const 1)
listLength2 = sumWith (\x -> 1)

--2.2.6
prod' :: Num a => [a] -> a
prod' [] = 0
prod' [x] = x
prod' (x:xs) = x * prod' xs

--5.2.1
import Data.List
sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

--5.2.2 nie działa!
sortDesc2 =  reverse . sort

--5.2.3 ?

--6.2.1
((,) $ 1) $ 2

--7.2.2
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x           = x : filter' p xs
    | otherwise     = filter' p xs


onlyEven' = filter' (\x -> x `mod` 2 == 0)
onlyOdd' = filter' (\x -> x `mod` 2 /= 0)
import Data.List
onlyUpper' = filter' isUpper

--7.2.3 
--ghci> length (filter even [1..10^6]) <- z biblioteki wbudowanej jest szybsze

--7.2.4
--length . onlyEven $ [1..10^2]
--length . filter even $ [1..10^6]

--7.2.5
length [x | x <- [1..10^6], even x]

--7.5.6
--filter (\s -> length s == 2) ["a", "aa", "aaa", "b", "bb"] ---> wybiere elementy tablicy o dlugosci 2
--filter (\(x,y) -> x > y) [(1,2), (2,2), (2,1), (2,2), (3,2)] --> wybiera pary gdzie wspolrzedna x (lewa) jest wieksza
--filter (\xs -> sum xs > 300) [[1..5], [56..60], [101..105]] --> wybiera elementy tablicy gdzie suma > 300
--length . filter (\f -> f 2 > 10) $ [(+5), (*5), (^5), \x -> 3 * x + 7] --> liczba funkcji z tablicy, których wart po wykonaniu argumentu 2 jes > 10

--8.2.2
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' = map' (* 2)
sqrElems'   = map' (^ 2)
lowerCase'   = map' toLower

--8.2.3
doubleElems'' l = [x * 2 | x <- l]
sqrElems'' l = [x ^ 2 | x <- l]
lowerCase'' l = [toLower x | x <- l]

--9.3.1
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' g = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

--9.3.2
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

sumWith''' g = foldl' (\acc x -> g x + acc) 0
prodWith''' g = foldl' (\acc x -> g x * acc) 1








