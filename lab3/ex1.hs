f1 = \x -> x - 2

f2 = \x y -> sqrt(x^2 + y^2)
f2' = \(x, y) -> sqrt(x^2 + y^2)

--f3 :: Int -> Int -> Int -> Double
f3 = \x y z -> sqrt(fromIntegral x^2 + fromIntegral y^2 + fromIntegral z^2)
f3' = \(x, y, z) -> sqrt(fromIntegral x^2 + fromIntegral y^2 + fromIntegral z^2)

f4 = \x -> 2 * x
f5 = \x -> x * 2
f6 = \x -> 2 ^ x
f7 = \x -> x ^ 2
f8 = \x -> 2 / x
f9 = \x -> x / 3
f10 = \x -> 4 - x

f11 = \x -> sqrt x
f12 = \x -> abs x
f13 = \x -> log x
f14 = \x -> x

f15 = \x y-> x

f16 = \x -> if x `mod` 2 == 0 then True else False
f16' = \x ->  x `mod` 2 == 0

f17 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f18 = \x -> if x == 1 then 3 else 0