module MyLib (main,process) where
solarSystem = "Sun 57909227 Mercury\n\
                    \Earth 384400 Moon\n\
                    \Sun 149598262 Earth\n\
                    \Moon 1757 LROrbiter\n\
                    \Mars 9376 Phobos\n\
                    \Mars 23458 Deimos\n\
                    \Sun 227943824 Mars\n\
                    \Sun 778340821 Jupiter\n\
                    \Sun 1426666422 Saturn\n\
                    \Sun 2870658186 Uranus\n\
                    \Sun 4498396441 Neptune\n\
                    \Mars Phobos\n\
                    \Mars Deimos\n\
                    \Sun\n\
                    \Mars\n\
                    \LROrbiter\n\
                    \Moon"
main :: IO ()
main = do
     contents <- getContents
     putStrLn $ process contents

-- takes an input string
-- divide input string in lines
-- call aux function
-- call dropData function
-- returns the output created by outputAux function
process :: String -> String
process inp = outputAux data1 data2
             where linesL = lines inp
                   data1 = aux linesL
                   data2 = dropData linesL

-- takes data and lines to create output
-- return the output string
outputAux ::  [(String,String,Int)] -> [[String]] -> String
outputAux _ [] = ""
outputAux data1 [h] | length h == 2 = distance data1 (h !! 0) (h !! 1)
                    | otherwise = creatLine data1 (head h)
outputAux data1 (h:t) | length h == 2 = distance data1 (h !! 0) (h !! 1) ++ "\n" ++ outputAux data1 t
                      | otherwise = creatLine data1 (head h) ++ "\n" ++ outputAux data1 t

-- takes the input lines
-- returns the data lines in the type -> list of (obj1,obj2,distance)
aux :: [String] -> [(String,String,Int)]
aux [] = []
aux (h:t) | length line == 3 = (line !! 0, line !! 2, read $ line !! 1) : aux t
          | otherwise = []
            where line = words h

-- takes the input lines
-- returns a list of list with the lines divided in words for the output lines
dropData :: [String] -> [[String]]
dropData [] = []
dropData (h:t) | length line < 3 = line : dropData t
               | otherwise = dropData t 
                 where line = words h 

-----------------------------------
-- takes data and 2 objects
-- creates a list with the objets and the object is orbits for the 2 objects
-- calculates the common object
-- calculates the distance to the common object and returns the sum of the 2 distances
distance :: [(String,String,Int)] -> String -> String -> String
distance data1 pl1 pl2 = ("From " ++ pl1 ++ " to " ++ pl2 ++ " is " ++ show (d1+d2) ++ "km")
                         where pl1Orb = pl1 : orbits data1 pl1
                               pl2Orb = pl2 : orbits data1 pl2
                               orbPl = sameOrbit pl1Orb pl2Orb
                               d1 = orbitsDistance data1 pl1Orb orbPl
                               d2 = orbitsDistance data1 pl2Orb orbPl

-- takes data, a list with an objet and the object it orbits, and a final object
-- return the distance between the start object and the final one
orbitsDistance :: [(String,String,Int)] -> [String] -> String -> Int
orbitsDistance _ [] _ = 0
orbitsDistance _ [x] _ = 0
orbitsDistance list (from:to:t) last | equalStr to last = planetsDistance list from to
                                     | equalStr from last = 0
                                     | otherwise = planetsDistance list from to + orbitsDistance list (to:t) last 

-- takes data and 2 objects and returns the distance between them
planetsDistance :: [(String,String,Int)] -> String -> String -> Int
planetsDistance [] _ _ = 0
planetsDistance ((pl1,pl2,d):t) from to | equalStr pl2 from && equalStr pl1 to = d
                                        | otherwise = planetsDistance t from to

-- takes 2 objects lists and returns the common object 
sameOrbit :: [String] -> [String] -> String
sameOrbit [] _ = ""
sameOrbit (h:t) list | inList h list = h
                     | otherwise = sameOrbit t list

-- takes a string and a list of string and returns true if the given string is in the list
inList :: String -> [String] -> Bool
inList _ [] = False
inList str (h:t) | equalStr str h = True
                 | otherwise = inList str t

---------------------------------           

creatLine :: [(String,String,Int)] -> String -> String
creatLine list pl | o == [] = pl ++ " orbits"
                  | otherwise = pl ++ " orbits" ++ listToStr o
                    where o = orbits list pl

-- converts a list of strings to a string
listToStr :: [String] -> String
listToStr [] = ""
listToStr (h:t) = " " ++ h ++ listToStr t

-- takes data and an object and returns a list of the objects it orbits
orbits :: [(String,String,Int)] -> String -> [String]
orbits list pl | length plOrbit == 0 = []
               | otherwise = plOrbit ++ orbits list (head plOrbit)
                 where plOrbit = orbitAux list pl


-- takes data and an object and returns a list with the object it obits directly or empty list
orbitAux :: [(String,String,Int)] -> String -> [String]
orbitAux [] pl = []
orbitAux ((pl1,pl2,d):t) pl | equalStr pl pl2 = [pl1]
                            | otherwise = orbitAux t pl 

-- compare 2 string and return true in case they are equal
equalStr :: String -> String -> Bool
equalStr [] [] = True
equalStr [] _ = False
equalStr _ [] = False
equalStr (h:t) (h2:t2) | h == h2 = equalStr t t2
                       | otherwise = False

