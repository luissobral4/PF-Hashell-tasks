findTheA :: [String] -> [String]
findTheA [] = []
findTheA (p@('A':s):t) = p : findTheA t
findTheA (h:t) = findTheA t

             