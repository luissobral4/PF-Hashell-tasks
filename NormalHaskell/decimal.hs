
duoToDec :: String -> Int
duoToDec s | length s > 2 && start [s !! 0,s !! 1] = value (reverse $ tail $ tail s) 0
           | otherwise = error "Program error: Not a valid duodecimal number: must start with '0d'"
             where
               len = length s


start :: String -> Bool
start s = s == "0d"

value :: String -> Int -> Int
value [] _ = 0
value (h:t) index | v < 0 = error "Program error: Not a valid duodecimal number: contains invalid digit"
                  | otherwise = v * 12^index + value t (index+1)
                   where
                      v = charToInt h


charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt 'A' = 10
charToInt 'B' = 11
charToInt _ = -1



decToDuo :: Int -> String
decToDuo n = "0d" ++ auxDec n

auxDec :: Int -> String
auxDec 0 = ""
auxDec n | r == 10 = auxDec (div n 12) ++ "A"
         | r == 11 = auxDec (div n 12) ++ "B"
         | otherwise = auxDec (div n 12) ++ show r
           where
             r = mod n 12

---------------------------------------------------------------


leftRight :: [Int] -> [Int]
leftRight [] = []
leftRight (h:t) | h == 1 || h == 2 = -1 : leftRight t
                | h == 9 || h == 10 = 1 : leftRight t
                | otherwise = 0 : leftRight t

---------------------------------------------------------------

frontBack :: [Int] -> [Int]
frontBack l = map auxFrontBack l

auxFrontBack :: Int -> Int
auxFrontBack 1 = -1
auxFrontBack 2 = -1
auxFrontBack 3 = -1
auxFrontBack _ = 1

---------------------------------------------------------------

takeStep :: [Int] -> [Int] -> [ (Int, Int) ]
takeStep [] _ = []
takeStep (h:t) (h2:t2) = (h,h2) : takeStep t t2

---------------------------------------------------------------

walkEnd :: [(Int,Int)] -> (Int,Int)
walkEnd [] = (0,0)
walkEnd (h:t) = auxWalk t h

auxWalk :: [(Int,Int)] -> (Int,Int) -> (Int,Int)
auxWalk [] p = p
auxWalk ((x,y):t) (x2,y2) = auxWalk t (x+x2,y+y2)

---------------------------------------------------------------

walkPath :: Int -> [(Int,Int)] -> [(Int,Int)]
walkPath x l = (0,0) : auxWalkPath x l (0,0)

auxWalkPath :: Int -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
auxWalkPath _ [] _ = []
auxWalkPath 0 ((x,y):t) (x2,y2) = [(x+x2,y+y2)]
auxWalkPath s ((x,y):t) (x2,y2) = new : auxWalkPath (s-1) t new
                                  where
                                    new = (x+x2,y+y2)
