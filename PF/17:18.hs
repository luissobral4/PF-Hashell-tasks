-- 1)
insert :: Ord a => a -> [a] -> [a]
insert  n [] = [n]
insert n (h:t) | n < h = h : insert n t
               | otherwise = n : h : t

-- 2)
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x : t) = x : catMaybes t
catMaybes (Nothing : t) = catMaybes t

--3)
data Exp a = Const a
           | Var String
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
  show (Const a) = show a
  show (Var a) = a
  show (Mais a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"

-- 4)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f l = inserton f (sortOn f (tail l)) (fst l)

inserton :: Ord b => (a -> b) -> [a] -> a -> [a]
inserton f [] l a = [a]
inserton f (h:t) n a | f n > f h = n : h : t
                     | otherwise = h : inserton f t n

-- 5)
-- a)
amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = maximum l - minimum l

-- b)
parte :: [Int] -> ([Int],[Int])
parte l = (take a x,drop a x)
          where a = div (amplitude l) 2
                x = sort l
-- 6)
data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

conta :: Imagem -> Int
conta (Quadrado x) = 1
conta (Mover p i) = conta i
conta (Juntar l) = sum (map (conta) l)

-- apaga :: Imagem -> IO Imagem
