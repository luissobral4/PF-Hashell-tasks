input = "Sun 57909227 Mercury\nEarth 384400 Moon\nSun 149598262 Earth\nMoon 1757 LROrbiter\nMars 9376 Phobos\nMars 23458 Deimos\nSun 227943824 Mars\nSun 778340821 Jupiter\nSun 1426666422 Saturn\nSun 2870658186 Uranus\nSun 4498396441 Neptune\nSun Moon\nDeimos Moon\nDeimos\nDeimos Phobos\nMoon\nLROrbiter"

output :: String -> String
output inp = outputAux data1 data2
             where linesL = lines inp
                   data1 = aux linesL
                   data2 = dropData linesL

outputAux ::  [(String,String,Int)] -> [[String]] -> String
outputAux _ [] = ""
outputAux data1 (h:t) | length h == 2 = distance data1 (h !! 0) (h !! 1) ++ "\n" ++ outputAux data1 t
                      | otherwise = (head h) ++ " orbits " ++ (listToStr $ orbits data1 (head h)) ++ "\n" ++ outputAux data1 t

aux :: [String] -> [(String,String,Int)]
aux [] = []
aux (h:t) | length line == 3 = (line !! 0, line !! 2, read $ line !! 1) : aux t
          | otherwise = []
            where line = words h


dropData :: [String] -> [[String]]
dropData [] = []
dropData (h:t) | length line < 3 = line : dropData t
               | otherwise = dropData t 
                 where line = words h 

-----------------------------------

distance :: [(String,String,Int)] -> String -> String -> String
distance data1 pl1 pl2 = ("From " ++ pl1 ++ " to " ++ pl2 ++ " is " ++ show (d1+d2) ++ "km")
                         where pl1Orb = pl1 : orbits data1 pl1
                               pl2Orb = pl2 : orbits data1 pl2
                               orbPl = sameOrbit pl1Orb pl2Orb
                               d1 = orbitsDistance data1 pl1Orb orbPl
                               d2 = orbitsDistance data1 pl2Orb orbPl

orbitsDistance :: [(String,String,Int)] -> [String] -> String -> Int
orbitsDistance _ [] _ = 0
orbitsDistance _ [x] _ = 0
orbitsDistance list (from:to:t) last | equalStr to last = planetsDistance list from to
                                     | otherwise = planetsDistance list from to + orbitsDistance list (to:t) last 

planetsDistance :: [(String,String,Int)] -> String -> String -> Int
planetsDistance [] _ _ = 0
planetsDistance ((pl1,pl2,d):t) from to | equalStr pl2 from && equalStr pl1 to = d
                                        | otherwise = planetsDistance t from to

sameOrbit :: [String] -> [String] -> String
sameOrbit [] _ = ""
sameOrbit (h:t) list | inList h list = h
                     | otherwise = sameOrbit t list

inList :: String -> [String] -> Bool
inList _ [] = False
inList str (h:t) | equalStr str h = True
                 | otherwise = inList str t

---------------------------------           

listToStr :: [String] -> String
listToStr [x] = x
listToStr (h:t) = h ++ " " ++ listToStr t

orbits :: [(String,String,Int)] -> String -> [String]
orbits list pl | length plOrbit == 0 = []
               | otherwise = plOrbit ++ orbits list (head plOrbit)
                 where plOrbit = orbitAux list pl



orbitAux :: [(String,String,Int)] -> String -> [String]
orbitAux [] pl = []
orbitAux ((pl1,pl2,d):t) pl | equalStr pl pl2 = [pl1]
                            | otherwise = orbitAux t pl 


equalStr :: String -> String -> Bool
equalStr [] [] = True
equalStr [] _ = False
equalStr _ [] = False
equalStr (h:t) (h2:t2) | h == h2 = equalStr t t2
                       | otherwise = False