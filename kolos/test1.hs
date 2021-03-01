--fn =  zip  .  map (  t2sl_minmax  ()  t2s_s_len ) 
--    where
--       t2s_s_len (s1, s2) = ((s1, length s1), (s2, length s2))
--       t2sl_minmax ((s1, l1), (s2, l2)) = if l1 <= l2 then (s1, s2) else (s2, s1)

---numOfEven :: Integral t => [t] -> Int
--numOfEven :: [Integer] -> Integer
--numOfEven = iter 0 
--  where iter acc [] = acc  -- the base case
--        iter acc (x:xs)  | even x = iter  (1 + acc)  xs
--                       | otherwise = iter acc xs-







--f :: (a -> b) -> [b]
--f = \g -> map g > ( \x -> map ($ x) [id, (+1), (+2)])
--a = foldr (+) 0 . map((^3).(+1)) . filter even . filter ((<13) . (*2))  $[1..10]

fact 0 = 1
fact n = n * fact n - 1

