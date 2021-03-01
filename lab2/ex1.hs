myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x, y) = x + y

add2C :: Num a => (a -> (a -> a))
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (a, b, c) = a + b + c

add3C :: Num a => (a -> (a -> (a -> a)))
add3C a b c = a + b + c

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f a b = f (a, b)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (a, b) = f a b

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = (subtract 5)

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f a b   =  f b a

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []
