chew :: String -> String
chew x = takeWhile (== head x) x

nibble :: String -> String
nibble = take 9 . chew

runs :: String -> [String]
runs "" = []
runs x = run : (runs $ drop size x)
         where run = nibble x 
               size = length run

encode :: String -> [(Char,Int)]
encode "" = [] 
encode x = (head run,size) : (encode $ drop size x)
            where run = nibble x 
                  size = length run

flatten :: [(Char,Int)] -> String
flatten [] = ""
flatten ((c,s):t) = c : show s ++ flatten t 

compress :: String -> String
compress = flatten . encode

decode:: [(Char, Int)] -> String
decode [] = ""
decode ((c,s):t) = replicate s c ++ decode t

expand :: String -> [(Char, Int)]
expand "" = []
expand [x] = []
expand (f:s:t) = (f,size) : expand t
                 where size = read [s]

decompress :: String -> String
decompress = decode . expand
