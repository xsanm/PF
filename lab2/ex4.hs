isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx idx tmp = head (drop (idx - 1) tmp)


isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []