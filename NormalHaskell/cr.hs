data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show,Eq)

--1
ignoreCaseCount :: Char -> [Char] -> Int
ignoreCaseCount c l = length $ filter (== toLower c) $ map (toLower) l

--2 need to include ignoreCaseCount code
duplicateEncoder :: [Char] -> [Char]
duplicateEncoder l = map (f l) l
                    where f l x | ignoreCaseCount x l > 1 = '#'
                                | otherwise = '*'

--3
oddSum :: [Int] -> Bool
oddSum l = odd $ sum l

--7 don't know this one

--9 less then 30s in 1*10^8, try this
productOfThree :: Integer -> Maybe (Integer, Integer, Integer)
productOfThree n = aux n 2 3 (n `div` 6) (n `div` 6)

aux ::  Integer -> Integer -> Integer -> Integer -> Integer -> Maybe (Integer ,Integer , Integer)
aux n x y z max | x /= y && x /= z && y /= z && x * y * z == n = Just (x,y,z)
                | x * y * z < n = aux n x (y + 1) newM newM
                | z /= y + 1 = aux n x y (z - 1) max
                | y /= max || x * y * z < n = aux n x (y + 1) newM newM
                | x /= max = aux n (x + 1) (x + 2) newM newM
                | otherwise = Nothing
                  where newM = (n `div` (x * y))


--10
eraseZeros :: [Char] -> Int
eraseZeros l = zeroAux 0 l False

zeroAux :: Int -> [Char] -> Bool -> Int
zeroAux _ [] _ = 0
zeroAux c ('0':t) True = zeroAux (c + 1) t True
zeroAux c ('0':t) False = zeroAux c t False
zeroAux c ('1':t) True = c + zeroAux 0 t True
zeroAux c ('1':t) False = zeroAux 0 t True  


--12 find error in equal numbers, try this
comp :: [Integer] -> [Integer] -> Bool
comp [] [] = True
comp (h:t) l = (length $ filter (== h * h) l) == l2 && comp (filter (/= h) t) (filter (/= h * h) l)
               where l2 = length $ filter (== h) (h:t)



--19 don't know this one

--21 
postOrder :: Tree a -> [a]
postOrder Nil = []
postOrder (Node l a r) = postOrder l ++ postOrder r ++ [a]


--22 seems rigth, can you add an example when it fails?
isSymmetric :: (Eq a) => Tree a -> Bool
isSymmetric Nil = False
isSymmetric (Node l a r) = compareT l r

compareT :: (Eq a) => Tree a -> Tree a -> Bool
compareT Nil Nil = True
compareT Nil _ = False
compareT _ Nil = False
compareT (Node l a r) (Node l2 a2 r2) | a == a2 = compareT r l2 && compareT l r2
                                      | otherwise = False
