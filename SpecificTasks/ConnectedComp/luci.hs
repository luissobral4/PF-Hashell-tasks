m :: [[Int]]
m = [[0,0,0,0,1,1],
     [1,1,1,1,1,0],
     [1,1,0,0,1,0],
     [1,1,0,0,1,1],
     [1,0,1,1,1,1]]
 
 -- takes a initial matrix and a binary value
 -- call maxDF and returns size of the largest connected component
nlcc :: [[Int]] -> Int -> Int
nlcc matrix value = maxDF (0,0) 0 value (replicate size (replicate size2 (-1))) matrix
                                where size = length matrix
                                      size2 = length $ head matrix


-- counts size of a component
-- takes components matrix and the component int
-- returns component size
count :: [[Int]] -> Int -> Int
count [] _ = 0
count (h:t) c = length (filter (==c) h) + count t c

-- find the largest component
-- takes components matrix and the start component
-- returns the size of the largest component
maxComp :: [[Int]] -> Int -> Int
maxComp _ (-1) = 1
maxComp m c = max (count m c) (maxComp m (c-1))

-- calls dephfirst function for each position ignoring the ones already visited
-- takes a position (r,c), a number for the atual component, a binary value, a visited matrix (mVis) and the initial matrix
-- return the result of maxComp witch calculates the largest component for the visited matrix
maxDF :: (Int,Int) -> Int -> Int -> [[Int]] -> [[Int]] -> Int
maxDF (r,c) number binary visited m | r >= size = maxComp visited number
                                    | visitedPos || binary' = maxDF nextPos' number binary visited m
                                    | otherwise = maxDF nextPos' (number+1) binary (dephfirst (r,c) number binary m visited) m
                                      where size = length m
                                            size2 = length $ head m
                                            binary' = checkPos binary (r,c) m == False
                                            nextPos' = nextPos (r,c) size2
                                            visitedPos = checkPos (-1) (r,c) visited == False 

-- calculate the next position of a matrix
-- takes a matrix and the size of each line
-- return the next position
nextPos :: (Int,Int) -> Int -> (Int,Int)
nextPos (r,c) s | c >= s = (r+1,0)
                | otherwise = (r,c+1)

-- implements a dephfirst algorithm for a matrix. Starting in a given position and going to the connected position with the same binary value
-- takes a position (r,c), a number for the atual component, a binary value, the initial matrix and visited matrix (mVis) 
-- returns visited matrix with more visited positions
dephfirst :: (Int,Int) -> Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
dephfirst (r,c) number binary m mVis | visited || value = mVis
                                     | otherwise = dephfirst (r,c-1) number binary m $ 
                                                   dephfirst (r-1,c) number binary m $ 
                                                   dephfirst (r,c+1) number binary m $ 
                                                   dephfirst (r+1,c) number binary m mVis'
                                        where visited = checkPos (-1) (r,c) mVis == False 
                                              value = checkPos binary (r,c) m == False
                                              mVis' = changeComp (r,c) number mVis

-- check if element at given position == value
checkPos :: Int -> (Int,Int) -> [[Int]] -> Bool
checkPos value (r,c) matrix | r < 0 || r > size || c < 0 || c > size2 = False
                            | otherwise = ((matrix !! r) !! c) == value
                              where size = length matrix - 1
                                    size2 = length (head matrix) - 1

-- change component for the given position (row,column)
changeComp ::(Int,Int) -> Int -> [[Int]] -> [[Int]]
changeComp (r,c) comp m = take r m ++ ((take c rowR ++ (comp):drop (c+1) rowR)):drop (r+1) m
                            where s = length m
                                  rowR = m !! r  