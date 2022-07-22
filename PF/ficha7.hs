data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- 1)
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico x) = (-1)*calcula x
calcula (Mais x y) = calcula x + calcula y
calcula (Menos x y) = calcula x - calcula y
calcula (Mult x y) = calcula x * calcula y

infixa :: ExpInt -> String
infixa (Const x) = show x
infixa (Simetrico x) = "((-1)*" ++ infixa x ++ ")"
infixa (Mais x y) = "(" ++ infixa x ++ "+" ++ infixa y ++ ")"
infixa (Menos x y) = "(" ++ infixa x ++ "-" ++ infixa y ++ ")"
infixa (Mult x y) = "(" ++ infixa x ++ "*" ++ infixa y ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico x) = posfixa x ++ "(-1)*"
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ "+"
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++ "-"
posfixa (Mult x y) = posfixa x ++ " " ++ posfixa y ++ "*"

-- 2)

data RTree a = R a [RTree a]
             deriving Show

-- a)
soma :: Num a => RTree a -> a
soma (R x l) = x+sum (map soma l)

-- b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum (map altura l)

--aaa :: RTree a -> char -> R
--aux a (Term b ((Term x l):t)
-- c)
prune :: Int -> RTree a -> RTree a
--prune 0 (R r l) = (R r [])
prune 1 (R r _) = (R r [])
prune x (R r l) = (R r (map (prune (x-1))  l))

-- d)
mirror :: RTree a -> RTree a
mirror (R x l) = (R x (reverse (map mirror l)))

-- e)
postorder :: RTree a -> [a]
postorder (R x l) = concat (map postorder l) ++ [x]


-- 3)
data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork a b) = ltSum a + ltSum b

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

ltHeight :: LTree a -> Int
ltHeight (Tip c) = 1
ltHeight (Fork e d) = 1 + maximum (ltHeight e,ltHeight d)


-- 4)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
                deriving Show

t = No '4' (Leaf '6') (Leaf '3')

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree t = let b = aux1 t
                   f = aux2 t
               in (b,f)

aux1 (Leaf x) = Empty
aux1 (No x e d) = Node x (aux1 e) (aux1 d)

aux2 (Leaf x) = Tip x
aux2 (No x e d) = Fork (aux2 e) (aux2 d)

-- joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
-- joinTrees (Empty) (Tip x) = Just (Leaf x)
-- joinTrees (Node x e d) (Fork )
