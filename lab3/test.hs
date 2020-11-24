import Data.Char

a = foldr (+) 0 . map ((^3) . (+ 1)) . filter even . filter ((<13) . (*2)) $ [1..10]
b = sum [ (x + 1) ^ 3 | x <- [1..10], 2 * x < 13, even x]

--f g x = [g x, g (x + 1), g (x + 2)]

f = \g -> map g . (\x -> map ($ x) [id, (+1), (+2)])

fun :: String -> Int
fun = sum . map ((^2) . length) . filter (all isUpper) . filter((== 'K') . head) . words  