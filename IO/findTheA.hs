fintTheA :: [String] -> [String]
fintTheA [] = []
fintTheA (h:t) | head h == 'A' = h : fintTheA t
               | otherwise = fintTheA t

             