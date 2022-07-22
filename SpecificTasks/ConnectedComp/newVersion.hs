ma :: [[Int]]
ma = [[0,0,0,0,1,1],
     [1,1,1,1,1,0],
     [1,1,0,0,1,0],
     [1,1,0,0,1,1],
     [1,0,1,1,1,1]]
 

 -- take the initial matrix and a value
 -- use repDF function and return largest connected component
nlcc :: [[Int]] -> Int -> Int
nlcc matrix value = let rows = length matrix
                        columns = length (head matrix)
                    in repDF (rows,columns) 0 1 value (concat matrix) (replicate (rows*columns) 0)


-- largest component
-- takes component list and the component with the higest number
-- returns size of the largest component
largestComponent :: [Int] -> Int -> Int
largestComponent _ 0 = 1
largestComponent m c = max (length $ filter (==c) m) (largestComponent m (c-1))

-- for each position of the matrix calculates the largest component starting at that position
-- takes the size of the matriz (rows,columns), atual position, the atual component, a value, matrix list and componentsList
-- return the largest component size
repDF :: (Int,Int) -> Int -> Int -> Int -> [Int] -> [Int] -> Int
repDF s@(rs,cs) pos component value matrix compM = let val = isEqual value pos matrix
                                                       vis = isEqual 0 pos compM
                                                       pos' = pos + 1
                                                   in case () of
                                                       _ | pos >= rs * cs -> largestComponent compM component
                                                         | vis == False || val == False -> repDF s pos' component value matrix compM
                                                         | otherwise -> repDF s pos' (component+1) value matrix (dephfstList s pos component value matrix compM)


-- implements dephfirst algorithm for list representing a matrix.
-- takes the size of the matriz (rows,columns), atual position, the atual component, a value, matrix list and componentsList
-- return componentsList changed
dephfstList :: (Int,Int) -> Int -> Int -> Int -> [Int] -> [Int] -> [Int]
dephfstList s@(rs,cs) pos component value matrix compM = let vis = isEqual 0 pos compM
                                                             val = isEqual value pos matrix
                                                             compM' = setValue component pos compM
                                                         in case () of
                                                             _ | pos >= rs * cs || pos < 0 || vis == False || val == False -> compM
                                                               | mod pos cs == 0 -> dephfstList s (pos - cs) component value matrix $
                                                                                    dephfstList s (pos + cs) component value matrix $
                                                                                    dephfstList s (pos + 1) component value matrix compM'
                                                               | mod pos cs == cs-1 -> dephfstList s (pos - cs) component value matrix $
                                                                                    dephfstList s (pos + cs) component value matrix $
                                                                                    dephfstList s (pos - 1) component value matrix compM'
                                                               | otherwise -> dephfstList s (pos - cs) component value matrix $
                                                                              dephfstList s (pos + cs) component value matrix $
                                                                              dephfstList s (pos - 1) component value matrix $
                                                                              dephfstList s (pos + 1) component value matrix compM'


-- check if the element at position pos is equal to value 
isEqual :: Int -> Int -> [Int] -> Bool
isEqual value pos list = (list !! pos) == value

-- change component for the given position (row,column)
setValue :: Int -> Int -> [Int] -> [Int]
setValue value pos list = take pos list ++ value:drop (pos+1) list

