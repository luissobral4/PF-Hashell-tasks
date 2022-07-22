enumFromTo :: Int -> Int -> [Int]
enumFromTo a b | a == b = [a]
               | otherwise = a : enumFromTo (a+1)b
