array = [[0, 1, 1, 1, 0],[1, 1, 0, 0, 0],[1, 0, 0, 0, 1],[0, 0, 0, 0, 1],[1, 1, 1, 1, 0]]

getPos :: Integer -> Integer -> Integer -> [[Integer]] -> Bool
getPos row col value list | r < 0 || r > size || c < 0 || c > size = False
                          | otherwise = ((list !! r) !! c) == value
                            where size = length list - 1
                                  r = fromIntegral row
                                  c = fromIntegral col

getViz :: Integer -> Integer -> [[Bool]] -> Bool
getViz row col list = ((list !! fromIntegral row) !! fromIntegral col)

matrix :: Int -> Int -> Int -> [[Bool]]
matrix r c s = replicate r (replicate s False) ++ (replicate c False ++ True ++ replicate (s-c-1) False) ++ replicate (s-r-1) (replicate s False)

setTrue :: Int -> Int -> [[Bool]] -> [[Bool]]
setTrue r c l = take r l ++ (take c line ++ True ++ drop (s-c-1) line) ++ drop (s-r-1) l
                where s = length $ head l
                      line = l !! r 


bfs :: Integer -> Integer -> Integer -> [[Integer]] -> [[Bool]] -> [[Bool]]
bfs row col value list viz | !val || (!val1 && !val2 && !val3 && !val4) = viz
                           | !val1 && val2 && val3 && val4 = setTrue (r+1) c viz
                           | otherwise = viz
                             where size = length viz
                                   r = fromIntegral row
                                   c = fromIntegral col
                                   val = getPos r c value list && getViz (r-1) c viz
                                   val1 = getPos (r-1) c value list && !getViz (r-1) c viz
                                   val2 = getPos (r+1) c value list && !getViz (r+1) c viz
                                   val3 = getPos r (c+1) value list && !getViz row (c+1) viz
                                   val4 = getPos r (c-1) value list && !getViz row (c-1) viz

v :: Bool -> Bool -> Bool -> Bool -> Int -> Int -> [[Bool]] -> [[Bool]]
v True True True True r c viz = setTrue r (c-1) viz $ setTrue r (c+1) viz $ setTrue (r+1) c viz $ setTrue (r-1) c viz
v False True True True r c viz = setTrue r (c-1) viz $ setTrue r (c+1) viz $ setTrue (r+1) c viz
v True False True True r c viz = setTrue r (c-1) viz $ setTrue r (c+1) viz $ setTrue (r-1) c viz
v False True False True r c viz = setTrue r (c-1) viz $ setTrue (r+1) c viz $ setTrue (r-1) c viz
v False True True False r c viz = setTrue r (c+1) viz $ setTrue (r+1) c viz $ setTrue (r-1) c viz

v True True False False r c viz =setTrue (r+1) c viz $ setTrue (r-1) c viz
v False False True True r c viz = setTrue r (c-1) viz $ setTrue r (c+1) viz
v True False True False r c viz = setTrue r (c+1) viz $ setTrue (r-1) c viz
v False True False True r c viz = setTrue r (c-1) viz $ setTrue r (c+1) viz $ setTrue (r+1) c viz $ setTrue (r-1) c viz


--f = take 4 array ++ [1,1,1,1,1]