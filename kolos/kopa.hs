data D1 = D1{s::String, i::Int}

nonEmpty::String -> Maybe String
nonEmpty "" = Nothing
nonEmpty s = Just s

nonNegative :: Int -> Maybe Int
nonNegative n = if n>=0 then Just n else Nothing

validate :: D1 -> Maybe D1
validate d = if even $ i d then Just d else Nothing

mkD1::String -> Int -> Maybe D1
mkD1 s1 i1 = do
    s <- nonEmpty s1
    i <- nonNegative i1
    validate $ D1 s i



