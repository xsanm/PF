doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' = map' (* 2)
sqrElems'   = map' (^ 2)


sqrElems'' l = [x ^ 2 | x <- l]