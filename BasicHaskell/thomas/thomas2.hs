chew :: String -> String
chew x = takeWhile (== head x) x

nibble :: String -> String
nibble = take 9 . chew

runs :: String -> [String]
runs [] = []
runs s = let run = nibble s 
         in run : (runs (drop (length run) s))

encode :: String -> [(Char,Int)]
encode [] = []
encode s = let run = nibble s 
           in (head run,(length run)) : (encode (drop (length run) s))

flatten :: [(Char,Int)] -> String
flatten [] = []
flatten (h:t) = let str = fst h : show (snd h) 
                in str ++ flatten t

compress :: String -> String
compress s = flatten (encode s)


decode :: [(Char, Int)] -> String
decode [] = []
decode (h:t) = replicate (snd h) (fst h) ++ decode t


expand :: String -> [(Char, Int)]
expand [] = []
expand s = let c = s !! 0
               size = read [s !! 1]
           in (c,size) : expand (drop 2 s)

decompress :: String -> String
decompress s = decoded (expand s)