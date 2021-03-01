polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"


data Cart3DVec a = Cart3DVec a a a
xCoord (Cart3DVec a _ _) = a
yCoord (Cart3DVec _ a _) = a
zCoord (Cart3DVec _ _ a) = a


data Cart3DVec' a = Cart3DVec' {x::a, y::a, z::a}

data Shape = Circle Float |Rectangle Float Float
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b

data TrafficLights = Red | Orange | Green

actionFor :: TrafficLights -> String
actionFor Red = "STOP"
actionFor Orange = "READY"
actionFor Green = "GO"

data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt



data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)



eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2


show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving Show

--let tree1 = NodeBT 2 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT)
--let tree2 NodeBT 2 (NodeBT 2 EmptyBT EmptyBT) (NodeBT 3 EmptyBT EmptyBT)

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


instance Eq a => Eq (BinTree a) where
  (==) EmptyBT EmptyBT = True
  (==) EmptyBT _ = False
  (==) _ EmptyBT = False
  (==) (NodeBT a lt1 rt1) (NodeBT b lt2 rt2) = (a == b && lt1 == lt2 && rt1 == rt2)








