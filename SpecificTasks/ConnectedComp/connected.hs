getPos :: Int -> Int -> Int -> [[Int]] -> Bool
getPos row col value list | r < 0 || r > size || c < 0 || c > size = False
                          | otherwise = ((list !! r) !! c) == value
                            where size = length list - 1
                                  r = row
                                  c = col

setColor :: Int -> Int -> [[Int]] -> [[Int]]
setColor r c color l = take r l ++ (take c line ++ color ++ drop (s-c-1) line) ++ drop (s-r-1) l
                       where s = length $ head l
                             line = l !! r 

matriz = replicate 5 $ replicate 5 0
array = [[0, 1, 1, 1, 0],
		 [1, 1, 0, 0, 0],
		 [1, 0, 0, 0, 1],
		 [0, 0, 0, 0, 1],
		 [1, 1, 1, 1, 0]]

df :: Int -> (Int,Int) -> [[Int]] -> [[Int]] -> [[Int]]
df color (x,y) matriz viz | getPos x y 0 viz == False = viz
                          | otherwise = df color (x+1,y) matriz $ df color (x-1,y) matriz $ df color (x,y+1) matriz $ df color (x,y-1) matriz viz'
                          	where viz' = setColor x y color viz 