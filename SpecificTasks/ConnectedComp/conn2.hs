-- create the start visited matrix with a given size
visited :: Int -> Int -> [[Int]]
visited size size2 = replicate size $ replicate size2 0

array :: [[Int]]
array = [[0, 1, 1, 1, 0],
         [1, 1, 0, 0, 0],
         [1, 0, 0, 0, 1],
         [0, 0, 0, 0, 1],
         [1, 1, 1, 1, 0]]


-- takes a binary matrix and create a matrix with a color for each connected component 
largestConnected :: [[Int]] -> Int -> Int
largestConnected matrix value = maximum (listComponents components 1)
                                where components = largestConnectedAux (0,0) value 1 matrix (visited (length matrix) (length (head matrix)))


--count the length of one connected component
count :: [[Int]] -> Int -> Int
count [] _ = 0
count (h:t) c = length (filter (==c) h) + count t c


--creates a list with the count of each component
listComponents :: [[Int]] -> Int -> [Int]
listComponents m x | c < 1 = []
                   | otherwise = listComponents m (x+1) ++ [c]
                     where c = count m x

-- visit each position of the matrix
-- takes as arguments a position, a color, real matrix and visited matrix
-- if the position is already visited returns visited matrix
-- else calls recursion with another position and with the result of dephfirst starting on the argument position
largestConnectedAux :: (Int,Int) -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
largestConnectedAux (r,c) value color matrix viz | r >= size = viz
                                                 | c >= size2 = largestConnectedAux (r+1,0) value color matrix viz
                                                 | getPos (r,c) 0 viz == False || binary /= value = largestConnectedAux (r,c+1) value color matrix viz
                                                 | otherwise = largestConnectedAux (r,c+1) value (color+1) matrix (dephfirst binary color (r,c) matrix viz)
                                                   where size = length matrix
                                                         size2 = length (head matrix)
                                                         binary = get (r,c) matrix

-- dephfirst algorithm
-- takes as arguments a value, a color, a position, real matrix and visited matrix
-- if the position is already visited or have a different value returns visited matrix
-- else changes color of the argument position and visit the near positions
dephfirst :: Int -> Int -> (Int,Int) -> [[Int]] -> [[Int]] -> [[Int]]
dephfirst bin color (x,y) matriz viz | getPos (x,y) 0 viz == False || getPos (x,y) bin matriz == False = viz
                                     | otherwise = dephfirst bin color (x+1,y) matriz 
                                                   (dephfirst bin color (x-1,y) matriz 
                                                   (dephfirst bin color (x,y+1) matriz 
                                                   (dephfirst bin color (x,y-1) matriz viz')))
                                                   where viz' = setColor (x,y) color viz

-- verify is the element in the given position is equal to the given value
getPos :: (Int,Int) -> Int -> [[Int]] -> Bool
getPos (row,col) value list | r < 0 || r > size || c < 0 || c > size2 = False
                            | otherwise = ((list !! r) !! c) == value
                              where size = length list - 1
                                    size2 = length (head list) - 1
                                    r = row
                                    c = col

-- get the color on position (row,column) in the matrix
get :: (Int,Int) -> [[Int]] -> Int
get (r,c) list | r < 0 || r > size || c < 0 || c > size2 = -1
               | otherwise = ((list !! r) !! c)
                 where size = length list - 1
                       size2 = length (head list) - 1

-- replace the given position (row,column) in the matrix for the given color
setColor ::(Int,Int) -> Int -> [[Int]] -> [[Int]]
setColor (r,c) color l = take r l ++ [(take c line ++ [color] ++ drop (c+1) line)] ++ drop (r+1) l
                       where size = length l
                             line = l !! r  