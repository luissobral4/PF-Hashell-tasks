-- TESTE 16/17
type MSet a = [(a,Int)]

--a)
cardMSet :: MSet a -> Int
cardMSet a = sum (map (snd) a)

--b)
moda :: MSet a -> [a]
moda l = map (fst) (filter (\(x,y) -> y ==(maximum (map (snd) l))) l)

--c)
converteMSet :: MSet a -> [a]
converteMSet l = concat (map (\(x,y) -> replicate y x) l)

-- d)
addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] x y = [(x,y)]
addNcopies ((a,b):l) x y | b > y = (x,y):l
                         | otherwise = (a,b):addNcopies l x y

--2
data SReais = AA Double Double | FF Double Double
                               | AF Double Double | FA Double Double
                               | Uniao SReais SReais

--a)
instance Show SReais where
  show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
  show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
  show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]"
  show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
  show (Uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")"

--b)
pertence :: Double-> SReais -> Bool
pertence x (AA a b) = x > a && x < b
pertence x (FF a b) = x >= a && x <= b
pertence x (AF a b) = x > a && x <= b
pertence x (FA a b) = x >= a && x < b
pertence x (Uniao a b) = pertence x a || pertence x b

-- c)
tira :: Double -> SReais -> SReais
tira x (AA a b) | x > a && x < b = Uniao (AA a x) (AA x b)
                | otherwise = (AA a b)
tira x (FF a b) | x > a && x < b = Uniao (FA a x) (AF x b)
                | x == a = (AF a b)
                | b == x = (FA a b)
                | otherwise = (FF a b)
tira x (AF a b) | x > a && x < b = Uniao (AA a x) (AF x b)
                | x == b = (AA a b)
                | otherwise = (AF a b)
tira x (FA a b) | x > a && x < b = Uniao (FA a x) (AA x b)
                | x == a = (AA a b)
                | otherwise = (FA a b)
tira x (Uniao a b) = (Uniao (tira x a) (tira x b))


-- 3)
data RTree a = R a [RTree a]

--a)
percorre :: [Int] -> RTree a -> Maybe [a]
percorre a b = percorreC a b []

percorreC :: [Int] -> RTree a -> [a] -> Maybe [a]
percorreC [] r c = Just c
percorreC (h:t) (R a l) c | h > length l = Nothing
                          | otherwise = percorreC t (l !! h) (a:c)

-- -- b )
-- procura :: Eq a => a -> RTree a -> Maybe [Int]
-- procura a (R x []) = Nothing
-- procura a (R x l) | a == x = Just [x]
--                   | otherwise =

-- procuraC :: a -> [Int] -> RTree a -> [a]
-- procuraC n r c = Just c
-- percuraC n (R a l) c | h > length l = Nothing
--                       | otherwise = percorreC t (l !! h) (h:c))
--                           where m = map (percorreC )
