absInt :: Int -> Int
absInt n | n >= 0    = n
         | otherwise = -n

sgn :: Int -> Int
sgn a   | a < 0 = -1
        | a == 0 = 0
        | otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c)   | a <= b && a <= c = a
                    | b <= a && b <= c = b
                    | c <= a && c <= b = c

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