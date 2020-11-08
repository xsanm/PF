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