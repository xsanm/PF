--4.5.1
vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)

--4.5.2
swap :: (Int, Char) -> (Char, Int)
swap (i, c) = (c, i)

--4.5.3
threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x == y) && (x == z)

--4.5.4
triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt((a + b + c) * (a + b - c) * (a - b + c) * (b - a + c)) / 4

--5.3.1
absInt :: Int -> Int
absInt n = if n < 0
        then -n
        else n

--5.3.2
min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a < b
                then a
                else b

--5.3.3               
min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = if a <= b && a <= c
                    then a
                    else if b <= a && b <= c
                        then b
                        else c

--5.3.4
min3Int2 :: (Int, Int, Int) -> Int
min3Int2 (a, b, c) = min2Int(min2Int (a, b), c)

--5.3.5
toUpper :: Char -> Char
toUpper a = if a <= 'z' && a >= 'a'
        then toEnum(fromEnum a + (fromEnum 'A' - fromEnum 'a'))
        else a
toLower :: Char -> Char
toLower a = if a <= 'Z' && a >= 'A'
        then toEnum(fromEnum a - (fromEnum 'A' - fromEnum 'a'))
        else a

--5.3.6
isDigigt :: Char -> Bool
isDigigt a = (a <= '9' && a >= '0') 
charToNum :: Char -> Int
charToNum a = fromEnum(a) - fromEnum('0')

--6.5.1
sgn :: Int -> Int
sgn a   | a < 0 = -1
        | a == 0 = 0
        | otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c)   | a <= b && a <= c = a
                    | b <= a && b <= c = b
                    | c <= a && c <= b = c

--6.5.2
toUpper :: Char -> Char
toUpper a   | a <= 'z' && a >= 'a' = toEnum(fromEnum a + (fromEnum 'A' - fromEnum 'a'))
            | otherwise = a

toLower :: Char -> Char
toLower a   | a <= 'Z' && a >= 'A' = toEnum(fromEnum a - (fromEnum 'A' - fromEnum 'a'))
            | otherwise = a

isDigigt :: Char -> Bool
isDigigt a  | (a <= '9' && a >= '0')  = True
            | otherwise = False

charToNum :: Char -> Int
charToNum a = fromEnum(a) - fromEnum('0')

--7.7.1
or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' _              = True

and' :: (Bool, Bool) -> Bool
and' (True, True)   = True
and' _              = False

nand' :: (Bool, Bool) -> Bool
nand' (True, True)   = False
nand' _              = True

xor' :: (Bool, Bool) -> Bool
xor' (True, False)   = True
xor' (False, True)   = True
xor' _               = False

--8.5
isItTheAnswer :: String -> Bool
isItTheAnswer x =
    case x == "Love" of
    True -> True
    _ -> False

not' :: Bool -> Bool
not' b = case b of
          True  -> False
          _ -> True

or' :: (Bool, Bool) -> Bool
or' (a, b) = 
    case a || b of
        True -> True
        _    -> False

and' :: (Bool, Bool) -> Bool
and' (a, b) = 
    case a && b of
        True -> True
        _    -> False

nand' :: (Bool, Bool) -> Bool
nand' (a, b) = 
    case a && b of
        True -> False
        _    -> True

xor' :: (Bool, Bool) -> Bool
xor' (a, b) = 
    case (a /= b) of
        True -> True
        _    -> False

--9.3
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = (a / l, b / l)
    where l = sqrt(a^2 + b^2)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a, b, c) = (a / l, b / l, c / l)
    where l = sqrt(a^2 + b^2 + c^2)

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt(p * (p - a) * (p - b) * (p - c))
    where p = (a + b + c) / 2

--10.3
unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (a, b) = 
    let l = sqrt(a^2 + b^2)
    in (a / l, b / l)
    

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (a, b, c) = 
    let l = sqrt(a^2 + b^2 + c^2)
    in (a / l, b / l, c / l)

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = 
    let p = (a + b + c) / 2
    in sqrt(p * (p - a) * (p - b) * (p - c))

--11.9.1
roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where { d = sqrt (b * b - 4 * a * c); e = 2 * a }

--11.9.2
-- Komentarz jednoliniowy
{-
    Komentarz
    wieloliniowy
-}

--12.2
swap (x, y) = (y, x)

sgn a   | a < 0 = -1
        | a == 0 = 0
        | otherwise = 1

absInt x    | x < 0 = -x
            | otherwise = x

min2Int (x, y) = 
    if x < y 
        then x 
        else y

min3Int (x, y, z) = 
    if x < y && x < z 
        then x 
        else if y < x && y < z 
            then y 
            else z

threeEqual (x, y, z) = x == y && y == z