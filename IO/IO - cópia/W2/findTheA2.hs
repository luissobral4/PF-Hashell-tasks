findTheA :: [String] -> [String]
findTheA [] = []
findTheA ((h:s):t) | 'A' == h = (h:s) : findTheA t
                   | otherwise = findTheA t