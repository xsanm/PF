--Autor: Kamil Kurowski



--2.7.1
data Cart3DVec a = Cart3DVec a a a

xCoord (Cart3DVec a _ _) = a
yCoord (Cart3DVec _ a _) = a
zCoord (Cart3DVec _ _ a) = a


--2.7.2
data Cart3DVec a = Cart3DVec a a a

xCoord (Cart3DVec x _ _) = x
yCoord (Cart3DVec _ y _) = y
zCoord (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' {x::a, y::a, z::a}

--2.7.6
data Shape = Circle Float |Rectangle Float Float
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b

--2.7.7
data TrafficLights = Red | Orange | Green

actionFor :: TrafficLights -> String
actionFor Red = "STOP"
actionFor Orange = "READY"
actionFor Green = "GO"

--3.4.1
depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT a lt rt) = max (depthOfBT lt) (depthOfBT rt) + 1

flattenBTPreorder :: BinTree a -> [a]  -- preorder
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT a lt rt) = [a] ++ flattenBTPreorder lt ++ flattenBTPreorder rt

flattenBTInorder :: BinTree a -> [a]  -- inorder
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT a lt rt) = flattenBTInorder lt ++ [a] ++ flattenBTInorder rt

flattenBTPostorder :: BinTree a -> [a]  --postorder
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT a lt rt) = flattenBTPostorder lt  ++ flattenBTPostorder rt ++ [a]

mapBT :: (a -> b) -> BinTree a -> BinTree b 
mapBT _ EmptyBT = EmptyBT
mapBT foo (NodeBT a lt rt) = NodeBT (foo a) (mapBT foo lt) (mapBT foo rt)

--tu chyba mosi byc inaczej zeby przepiac poddrzewo
insert :: Ord a => a -> BinTree a -> BinTree a 
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT b lt rt)
    | a < b = NodeBT b (insert a lt) rt
    | a >= b = NodeBT b lt (insert a rt)

------
treeInsert :: (Ord a) => BinTree a -> a -> BinTree a 
treeInsert EmptyBT a = NodeBT a EmptyBT EmptyBT
treeInsert (NodeBT a lt rt) b
    | a == b = NodeBT a lt rt
    | a < b = NodeBT a (treeInsert lt b) rt
    | a > b = NodeBT a lt (treeInsert rt b)

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = treeInsert (list2BST xs) x  --PRZERTWARZA LISTE OD TYŁU


--3.4.6
data Expr a = Lit a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Mul (Expr a) (Expr a)

eval :: Num p => Expr p -> p
eval (Lit a) = a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b

show' :: Show a => Expr a -> String
show' (Lit a) = show a
show' (Add a b) = "(" ++ show' a ++ "+" ++ show' b ++ ")"
show' (Sub a b) = "(" ++ show' a ++ "-" ++ show' b ++ ")"
show' (Mul a b) = "(" ++ show' a ++ "*" ++ show' b ++ ")"

--5.6.2
instance Eq a => Eq (BinTree a) where
  (==) EmptyBT EmptyBT = True
  (==) EmptyBT _ = False
  (==) _ EmptyBT = False
  (==) (NodeBT a lt1 rt1) (NodeBT b lt2 rt2) = (a == b && lt1 == lt2 && rt1 == rt2)

  
