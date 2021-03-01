sgn :: Int -> Int
sgn n = if n < 0
        then - 1
        else if n == 0
            then 0
            else 1

absInt :: Int -> Int
absInt n = if n < 0
        then -n
        else n

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a < b
                then a
                else b

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = if a <= b && a <= c
                    then a
                    else if b <= a && b <= c
                        then b
                        else c

min3Int2 :: (Int, Int, Int) -> Int
min3Int2 (a, b, c) = min2Int(min2Int (a, b), c)

toUpper :: Char -> Char
toUpper a = if a <= 'z' && a >= 'a'
        then toEnum(fromEnum a + (fromEnum 'A' - fromEnum 'a'))
        else a

toLower :: Char -> Char
toLower a = if a <= 'Z' && a >= 'A'
        then toEnum(fromEnum a - (fromEnum 'A' - fromEnum 'a'))
        else a

isDigigt :: Char -> Bool
isDigigt a = (a <= '9' && a >= '0') 

charToNum :: Char -> Int
charToNum a = fromEnum(a) - fromEnum('0')