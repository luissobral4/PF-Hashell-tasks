data Tree a 
    = Tip
    | Bin (Tree a) a (Tree a)
    deriving (Show, Eq)


foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
foldTree f z Tip         = z
foldTree f z (Bin l x r) = f (foldTree f z l) x (foldTree f z r)


sumTree :: (Num a) => Tree a -> a
sumTree = foldTree f z
    where
    -- f :: a -> a -> a -> a
    f a b c = a + b + c
    -- hint: you will most likely rewrite this in the form "f x y z = something"

    -- z :: a
    z = 0

minTree :: (Ord a) => Tree a -> Maybe a
minTree = foldTree f z
    where
    f l y r | l == Nothing && r == Nothing = Just y
            | l == Nothing = Just (min y (toInt r))
            | r == Nothing = Just (min y (toInt l))
            | otherwise = Just (min y (min (toInt l) (toInt r)))
    z = Nothing

toInt :: Maybe a -> a
toInt (Just x) = x


listTree :: Tree a -> [a]
listTree = foldTree f z
    where
    f x y z = x ++ (y : z)
    z = []


 -- Part II
-- -------

data Rose a = Node a [Rose a]
    deriving (Show, Eq)

-- foldRose is the fold over a rose tree
foldRose :: (a -> [b] -> b) -> Rose a -> b
foldRose f (Node x l) = f x (map (foldRose f) l)

sumRose :: (Num a) => Rose a -> a
sumRose = foldRose f
    where
    f x l = (foldr (+) 0 l) + x

listRose :: Rose a -> [a]
listRose = foldRose f
    where
    f x l = x : concat l



-- PART III
-- --------------

-- Determine an appropriate type for minRose and write it using foldRose
minRose :: (Ord a, Num a) => Rose a -> Maybe a
minRose = foldRose f
    where
    f x list | list == [] = Just x
             | otherwise = Just (min x (foldr min (head l) l))
             where
             l = (map (\(Just s) -> s) list)


-- PART IV
-- ---------------

-- It is possible to define listTree so that it runs in linear time, but it requires
-- thinking about folds and their return values in a way that is not obvious. Can you
-- solve this riddle and find appropriate definitions?

listTree' :: Tree a -> [a]
listTree' t = foldTree f z t
    where
    f a b c = a ++ [b] ++ c
    z = []
