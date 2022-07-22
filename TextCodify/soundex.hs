charToDigit :: Char -> Char
charToDigit c | c == 'B' || c == 'F' || c == 'P' || c == 'V' = '1'
              | c == 'C' || c == 'G' || c == 'J' || c == 'K' || c == 'Q' || c == 'S' || c == 'X' || c == 'Z' = '2'
              | c == 'D' || c == 'T' = '3'
              | c == 'L' = '4'
              | c == 'M' || c == 'N' = '5'
              | c == 'R' = '6'
              | otherwise = '0'


remDup :: [Char] -> [Char]
remDup [] = []
remDup (h:t) = h : remDup ( remAux t h )


remAux :: [Char] -> Char -> [Char]
remAux [] c = []
remAux (h:t) c | h == c = remAux t c
               | otherwise = h : remAux t c 

remLetters2 :: [Char] -> [Char]
remLetters2 [] = []
remLetters2 (c:t) | c == 'A' || c == 'E' || c == 'I' || c == 'O' || c == 'U' || c == 'Y' = remLetters2 t
                  | otherwise = c : remLetters2 t 

remLetters :: [Char] -> [Char]
remLetters [] = []
remLetters (c:t) | c == 'H' || c == 'W' = remLetters t
                 | otherwise = c : remLetters t 

checkLenght :: [Char] -> [Char]
checkLenght l | len < 3 = checkLenght (l ++ ['0'])
              | otherwise = take 3 l
               where len = length l

soundex :: [Char] -> [Char]
soundex (h:t) = h : checkLenght ( map charToDigit word )
                where word = remLetters2 $ remDup $ remLetters t