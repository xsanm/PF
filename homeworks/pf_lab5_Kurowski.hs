-- Autor: Kamil Kurowski

--2.8.1
echo3' :: IO ()
echo3' = do
  l1 <- getLine
  l2 <- getLine 
  putStrLn $ l1 ++ l2

dialog' :: IO ()
dialog' = do
  putStr "What is your happy number? "
  n <- getLine 
  let num = read n :: Int
  if num == 7
    then putStrLn "Ah, lucky 7!"
    else if odd num
        then putStrLn "Odd number! That's most people's choice..."
        else putStrLn "Hm, even number? Unusual!"


--2.8.2
twoQuestions' :: IO ()
twoQuestions' = putStr "What is your name? " 
  >> getLine >>= \name -> putStr "How old are you? "
  >> getLine >>= \age -> print (name,age)

--6.8.1
--data MyList a = EmptyList | Cons a (MyList a) deriving Functor

--6.8.2
data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show, Functor)
instance Functor BinTree where
  fmap _ EmptyBT = EmptyBT
  fmap f (NodeBT v l r) = NodeBT (f v) (fmap f l) (fmap f r)


--8.4.1
newtype MyTriple a = MyTriple (a,a,a) deriving Show
instance Functor MyTriple where
  fmap f (MyTriple (a, b, c)) = MyTriple (f a, f b, f c)
  
instance Applicative MyTriple where
  pure f = MyTriple (f, f, f)
  MyTriple (f1, f2, f3) <*> MyTriple (v1, v2, v3) = MyTriple (f1 v1, f2 v2, f3 v3)