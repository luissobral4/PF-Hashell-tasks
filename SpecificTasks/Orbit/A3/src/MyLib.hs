module MyLib (main,process) where

main :: IO ()
main = do
     contents <- getContents
     putStrLn $ process contents

process :: String -> String
process s = let distances = loadDistances s
                myLines = map words (lines s)
                outLines = filter (\x -> length x<3) myLines
            in createOutput distances outLines

-------------------------------------------------------------------------

loadDistances :: String -> [(String,Int,String)]
loadDistances [] = []
loadDistances input = let myLines = lines input
                          myWords = map words myLines
                          distances = filter (\x -> length x==3) myWords
                      in map auxLine distances

auxLine :: [String] -> (String,Int,String)
auxLine [] = ("",-1,"")
auxLine (h:h2:h3:[]) = (h,read h2::Int,h3)


createOutput :: [(String,Int,String)] -> [[String]] -> String
createOutput _ [] = ""
createOutput distances l@(h:t) | size == 1 = hd ++ " orbits" ++ createOrbits distances hd ++ "\n" ++ createOutput distances t
                               | otherwise = objectsDistance distances hd (h!!1) ++ "\n" ++ createOutput distances t
                                 where size = length h
                                       hd = head h


                               

-------------------------------------------------------------------------

createOrbits ::  [(String,Int,String)] -> String -> String
createOrbits distances obj | obj2 == "" = ""
                           | otherwise = " " ++ obj2 ++ createOrbits distances obj2
                             where obj2 = findOrbit distances obj

findOrbit :: [(String,Int,String)] -> String -> String
findOrbit [] pl = ""
findOrbit ((obj1,_,obj2):t) obj | obj == obj2 = obj1
                                | otherwise = findOrbit t obj 

-------------------------------------------------------------------------

objectsDistance :: [(String,Int,String)] -> String -> String -> String
objectsDistance distances obj1 obj2 = let list1 = obj1:findOrbitList distances obj1
                                          list2 = obj2:findOrbitList distances obj2
                                          common = findCommonObj list1 list2
                                          dist = calculateOrbit distances obj1 common + calculateOrbit distances obj2 common
                                       in "From " ++ obj1 ++ " to " ++ obj2 ++ " is " ++ show (dist) ++ "km"

calculateOrbit :: [(String,Int,String)] -> String -> String -> Int
calculateOrbit distances obj objFinal | obj == objFinal = 0
                                      | otherwise = dist + calculateOrbit distances obj2 objFinal
                                        where (dist,obj2) = calculateDistance distances obj


calculateDistance :: [(String,Int,String)] -> String -> (Int,String)
calculateDistance distances obj | obj == obj' = (distance,obj2)
                                | otherwise = calculateDistance (tail distances) obj
                                  where (obj2,distance,obj') = head distances

findOrbitList :: [(String,Int,String)] -> String -> [String]
findOrbitList distances obj | obj2 == "" = []
                            | otherwise = obj2 : findOrbitList distances obj2
                              where obj2 = findOrbit distances obj


findCommonObj :: [String] -> [String] -> String
findCommonObj [] _ = ""
findCommonObj l1 l2 | any (==head l1) l2 = head l1
                    | otherwise = findCommonObj (tail l1) l2



