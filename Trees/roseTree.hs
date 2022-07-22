data Rose a = Node a [Rose a]
    deriving (Show, Eq)
-- foldRose is the fold over a rose tree
foldRose :: (a -> [b] -> b) -> Rose a -> b
foldRose f (Node n list) = f n (map (foldRose f) list)

sumRose :: (Num a) => Rose a -> a
sumRose = foldRose f
    where
    f n list = n + foldr (+) 0 list

-- listRose t returns all values in pre-order (i.e. the value at the node occurs 
--before any child values)
listRose :: Rose a -> [a]
listRose = foldRose f
    where
    f n list = n : foldr (++) [] list

minRose :: (Ord a, Num a) => Rose a -> Maybe a
minRose = foldRose f
    where
    f n list | list == [] = Just n
             | otherwise = Just (min n (minimum (map (\(Just e) -> e) list)))
