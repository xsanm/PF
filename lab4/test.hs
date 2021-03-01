--1
data Box a = MkBox { value::a }

instance Eq a => Eq (Box a) where
    MkBox {value :: v} == MkBox {value = v'} = v == v' 

data Tree a = Node (Tree a) a (Tree a) | Leaf

--2
sumSq :: Num a => Tree a -> a
--sumSq Leaf x = 0
--sumSq (Node left x right) = x^2 ++ sumSq left ++ sumSq right
sumSq Leaf = 0
sumSq (Node left x right) = x^2 + sumSq left + sumSq right

--3
data Data = Data {first::String, second::String}