not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer _      = False
isItTheAnswer "Love" = True -- :)

or' :: (Bool, Bool) -> Bool
or' (a, b) = a || b

and' :: (Bool, Bool) -> Bool
and' (a, b) = a && b

nand' :: (Bool, Bool) -> Bool
nand' (a, b) = not (a && b)
