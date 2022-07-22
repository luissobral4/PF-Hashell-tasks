-- 1
--a
myany :: (a -> Bool) -> [a] -> Bool
myany f [] = False
myany f (h:t) | f h = True
              | otherwise = myany f t

--b
myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f [] _ = []
myzipWith f _ [] = []
myzipWith f (h:t) (x:y) = (f h x):myzipWith f t y

--c
mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile f [] = []
mytakeWhile f (h:t) | f h = h : mytakeWhile f t
                    | otherwise = []

--d
mydropWhile  :: (a->Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f l@(h:t) | f h = mydropWhile f t
                      | otherwise = l

--e
myspan :: (a-> Bool) -> [a] -> ([a],[a])
myspan f [] = ([],[])
myspan f (h:t) | f h = (h:xs,ys)
               | otherwise = ([],t)
              where (xs,ys) = span f t

--f
mydeleteBy :: (a -> Bool) -> [a] -> [a]
mydeleteBy f [] = []
mydeleteBy f (h:t) | f h = t
                   | otherwise = h : mydeleteBy f t

deleteBy2 :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy2 f _ [] = []
deleteBy2 f x (h:t) | f x h = t
                    | otherwise = h : deleteBy2 f x t

--g
mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn f [] = []
mysortOn f (h:t) = insertOn f h (mysortOn f t)
                 where insertOn f h [] = [h]
                       insertOn f x (h:t) | f x <= f h = x:h:t
                                          | otherwise = h:insertOn f x t

--2
--a
type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = myfilter (\(x,y) -> y == n) p
-- selgrau i [] = []
--selgrau i l = mytakeWhile f l ++mytakeWhile f (mydropWhile f l)
--              where f (x,y) = y == i

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (h:t) | f h = h : myfilter f t
                 | otherwise = myfilter f t

--b
conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p)

--c
grau :: Polinomio -> Int
grau l = maximum (map snd l)

--d
deriv :: Polinomio -> Polinomio
deriv p = map (\(x,y) -> ((fromIntegral y)*x, (fromIntegral y)-1)) p
-- deviv p@((x,y):t) = (map (f p)
--              where f (x,y) = ((fromIntegral y)*x,(fromIntegral y)-1)

--e
calcula :: Float -> Polinomio -> Float
calcula x p = foldr (+) 0 (map (\(a,b) -> (a*x^b)) p)

--f
simp :: Polinomio -> Polinomio
simp p = myfilter (\(x,y) -> y /= 0) p

--g
mult :: Monomio -> Polinomio -> Polinomio
mult (x,y) p = map (\(a,b) -> (a*x,b+y)) p

--h
ordena :: Polinomio -> Polinomio
ordena p = mysortOn (\(a,b) -> b) p

--i
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza p@((a,b):t) = let l = myfilter (\(x,y) -> y == b) p
                            d = myfilter (\(x,y) -> y /= b) p
                            w = sum (map (fst) l)
                            in (w,b) : normaliza d

--j
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = myzipWith (\(a,b) (x,y) -> (a+x,b)) p1 p2

--k
produto :: Polinomio -> Polinomio -> Polinomio
produto [] p = []
produto p1 p2 = mult (head p1) p2 ++ produto (tail p1) p2

--l
equiv :: Polinomio -> Polinomio -> Bool
equiv [] [] = True
equiv p1 p2 | a == b = equiv d c
            | otherwise = False
            where (a:d) = ordena (normaliza p1)
                  (b:c) = ordena (normaliza p2)
--3
type Mat a = [[a]]
--a
dimOK :: Mat a -> Bool
dimOK a@(h:t) = myany (\x -> length x /= length h) a == False

--b
dimMat :: Mat a -> (Int,Int)
dimMat a = (length a,length (head a))

--c
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (h1:t1) (h2:t2) = myzipWith (+) h1 h2 : addMat t1 t2

--d
transpose :: Mat a -> Mat a
transpose [] = []
transpose a | length (head a) == 1 = [(reverse (map head a))]
            | otherwise = (reverse (map head a)) : transpose (map tail a)

--e
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat [] m = []
multMat m1@(h1:t1) m2@(h2:t2) = linha h1 m2 : multMat t1 m2

linha h1 m2 | length (head m2) == 1 = [(sum (myzipWith (*) h1 (map head m2)))]
            | otherwise = (sum (myzipWith (*) h1 (map head m2))) : linha h1 (map tail m2)

--f
-- zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
-- zipWMat

--g
--triSup :: Num a => Mat a -> Bool

--h
rotateLeft :: Mat a -> Mat a
rotateLeft [] = []
rotateLeft a | length (head a) == 1 = [map head a]
             | otherwise = (map head a) : rotateLeft (map tail a)
