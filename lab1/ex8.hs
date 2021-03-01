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