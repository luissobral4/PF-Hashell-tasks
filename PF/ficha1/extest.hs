--1  
enum :: Int->Int->[Int]
enum x y | x<y = x:enum (x+1) y
         | x>y = []
         | otherwise = [x]

--2
enum2 ::Int->Int->Int->[Int]
enum2 x y z | (y-x>0 && x>z) || (y-x<0 && x<z) = []
            | x==y = [x]
            | otherwise = x:enum2 y (y+y-x) z

--3  
mais :: [a]->[a]->[a]
mais (h:s) (x:y)=h:x:mais s y
mais [] (x:y) =(x:y)
mais (x:y) []=(x:y)

--4
pos1 :: [a]->Int->a
pos1 (h:s) 0 = h
pos1 (h:s) x = pos1 s (x-1)

--5
reverse1 :: [a]->[a]
reverse1 l =g l []


g l (h:s)=g (h:l) s
g l []=l 
--rever []=[]
--rever (h:s)=rever s++[h]

--6
take1 :: Int->[a]->[a]
take1 x [] = []
take1 0 (h:s)=[]
take1 x (h:s) |x>=length (h:s)=(h:s)
              |otherwise = h:take1 (x-1) s

--7
drop1 :: Int->[a]->[a]
drop1 0 l=l
drop1 x (h:s) |x>=length (h:s)=[]
              |otherwise = drop1 (x-1) s

--8
zip1 :: [a]->[b]->[(a,b)]
zip1 [] l=[]
zip1 l []=[]
zip1 (h:s)(x:y)=(h,x):zip1 s y

--9
elem1 ::Eq a=>a->[a]->Bool
elem1 x (h:s)=if(x==h)then True else elem1 x s

--10
replicate1 :: Int->a->[a]
replicate1 0 x = []
replicate1 n x=x:replicate1 (n-1) x

--11
inters :: a->[a]->[a]
inters a []=[]
inters a [x]=[x]
inters a (h:s)=h:a:inters a s

--12
group1 :: Eq a =>[a]->[[a]]
group1 []=[]
group1 l =x2(l):group1 (drop1(length(x2(l)))(l))


x2 :: Eq a =>[a]->[a]
x2 []=[]
x2 [x]=[x]
x2 (h:s:t)= if(h==s)then h:x2(s:t) else [h]


--13
concat1 ::[[a]]->[a]
concat1 []=[]
concat1 (h:s)=h++concat1 s

--14

inits1 :: [a]->[[a]]
inits1 []=[[]]
inits1 l = x5 0 l  


x5 :: Int -> [a] -> [[a]]
x5 x l | x < length (l) = (take1 x l):(x5 (x+1) l)
       | otherwise = [l]

--15
tails1 :: [a]->[[a]]
tails1 []=[[]]
tails1 (h:s)=(h:s):tails1 s
--16
isprefix ::Eq a=>[a]->[a]->Bool
isprefix l [] = False
isprefix [] l=True
isprefix (h:s)(x:y) |h==x=isprefix s y
                    |otherwise = False

--17
issuffix :: Eq a=>[a]->[a]->Bool
issuffix [] []=True
issuffix l []=False
issuffix [] l=False
issuffix (h:s)(x:y)|h==x=issuffix s y
                   |otherwise =issuffix (h:s)y

--18
elemin :: Eq a=>[a]->[a]->Bool
elemin [][]=True
elemin [] l=True
elemin l []=False
elemin (h:s)(x:y)|h==x=elemin s y
                 |otherwise =elemin (h:s)y

--19
elemind :: Eq a =>a->[a]->[Int]
elemind x [] = []
elemind x l =pos2 0 x l


pos2 :: Eq a => Int -> a -> [a] -> [Int]
pos2 r x []= []
pos2 r x (h:s) | x == h = r:pos2 (r+1) x s
             | otherwise = pos2(r+1) x s                  

--20
nub1 :: Eq a=>[a]->[a]
nub1 [] = []
nub1 (h:s) = h:nub1 (aux h s)


aux :: Eq a => a ->[a] -> [a]
aux x [] = []
aux x (h:s) | x==h = aux x s
            | otherwise = h:aux x s 

--21
delete1 :: Eq a=>a->[a]->[a]
delete1 x []=[]
delete1 x (h:s) |x==h=s
                |otherwise =h:delete1 (x) (s)

--22

del2 ::Eq a=>[a]->[a]->[a]
del2 [] []= []
del2 l []=l
del2 [] l=[]
del2 (h:s)(x:y) = del2 (delete1 x (h:s)) y
--23
union1 :: Eq a=>[a]->[a]->[a]
union1 [] []=[]
union1 [] l = l
union1 l []=l
union1 (h:s)(x:y) = union1 (h:s) y ++aux2 (h:s) x 

aux2 :: Eq a=>[a]-> a -> [a]
aux2 [] x = [x]
aux2 (h:s) x | x == h = []
             | otherwise = aux2 s x

--24
interset ::Eq a=>[a]->[a]->[a]
interset [] l=[]
interset l []=[]
interset (h:s)(x:y) = aux3 (x:y) h ++ interset s (x:y)


aux3 :: Eq a => [a]-> a -> [a]
aux3 [] x = []
aux3 (h:s) x | x== h = [x]
             | otherwise = aux3 s x

--25
insert ::Ord a=>a->[a]->[a]
insert x []=[x]
insert x (h:s) | x<h=(x:h:s)
               | otherwise = h:insert x s

--26
unwords1 :: [String]->String
unwords1 []=""
unwords1 [x]= x
unwords1 (x:s) = x++" "++unwords1 s

--27
unlines1 :: [String]->String
unlines1 []=[]
unlines1 (x:s) = x++"\n"++unlines s

--28
pMaior :: Ord a => [a] -> Int
pMaior [] = 0
pMaior (h:s) = aux5 0 (aux4 h s) (h:s)


aux4 :: Ord a => a -> [a] ->a
aux4 e [] = e
aux4 e (h:s) | h > e = aux4 h s
             | otherwise = aux4 e s

aux5 :: Ord a => Int -> a -> [a] -> Int
aux5 x y [] = 0
aux5 x y (h:s) | y== h = x
               | otherwise = aux5 (x+1) y s 

--29
temrep :: Eq a => [a] -> Bool
temrep [] = True
temrep (h:s) | aux6 h s ==True = True
             | otherwise = temrep s

aux6 :: Eq a => a -> [a] ->Bool
aux6 x [] = False
aux6 x (h:s) | x == h = True
             | otherwise = aux6 x s

--30
algarism :: [Char] -> [Char]
algarism [] = []
algarism (h:s) | h>='0' && h<='9'=h:algarism s
               | otherwise = algarism s
-- algarism (h:s) | (h =='0')= h:algarism s
--                | (h =='1')= h:algarism s
--                | (h =='2')= h:algarism s
--                | (h =='3')= h:algarism s
--                | (h =='4')= h:algarism s
--                | (h =='5')= h:algarism s
--                | (h =='6')= h:algarism s
--                | (h =='7')= h:algarism s
--                | (h =='8')= h:algarism s
--                | (h =='9')= h:algarism s
--                | otherwise = algarism s

--31
posimp :: [a]->[a]
posimp []=[]
posimp [x]=[]
posimp (h:s:t)=s:posimp t

--32
pospares :: [a] -> [a]
pospares [] = [] 
pospares [x] = [x]
pospares (h:s:t) = h : pospares t

--33
myisSorted :: Ord a => [a] -> Bool
myisSorted [x]= True
myisSorted (h:s:t) | h <= s = myisSorted (s:t)
                 | otherwise = False  

--34
myiSort :: Ord a => [a] -> [a]
myiSort [] = []
myiSort (h:s) = x3 h (myiSort s) 

x3 :: Ord a => a -> [a] -> [a]
x3 x [] = [x]
x3 x (h:s) | x > h = h:x3 x s
           | otherwise = x:(h:s)

--35
mymenor :: String -> String -> Bool
mymenor [] l = True
mymenor l [] = False
mymenor (a:b) (c:d) | a<c = True
                    | a==c = mymenor b d
                    | otherwise = False

--36
myelemSet :: Eq a => a -> [(a,Int)] -> Bool
myelemSet a [] = False
myelemSet a ((x,y):t) | a == x = True
                      | otherwise = myelemSet a t 

--37
mylenghtMSet :: [(a,Int)] -> Int
mylenghtMSet [] = 0
mylenghtMSet ((x,y):t) = y + mylenghtMSet t

--38
myconvertMSet :: [(a,Int)] -> [a]
myconvertMSet [] = []
myconvertMSet ((x,y):t) = (x4 x y) ++ myconvertMSet t

x4 :: a -> Int -> [a]
x4 x 0 = []
x4 x y = x:x4 x (y-1)

--39
myinsereMset :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myinsereMset a [] = []
myinsereMset a ((x,y):t) | a == x = ((x,y+1):t)
                         | otherwise = (x,y) : myinsereMset a t

--40
myremoveSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
myremoveSet a [] = []
myremoveSet a ((x,y):t) | a == x = t
                         | otherwise = (x,y):myremoveSet a t

--41
myconstroiMSet :: Ord a => [a] -> [(a,Int)]
myconstroiMSet [] = []
myconstroiMSet (h:t) = aux7 h 1 t

aux7 :: Ord a => a -> Int -> [a] -> [(a,Int)]
aux7 h c [] = [(h,c)]
aux7 h c (x:rt) | h == x    = aux7 h (c+1) rt
                | otherwise = (h,c):aux7 x 1 rt 

--42


--43


--44
data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int) 
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:hs) = posicao (x,y+1) hs
posicao (x,y) (Sul:hs) = posicao (x,y-1) hs
posicao (x,y) (Este:hs) = posicao (x+1,y) hs
posicao (x,y) (Oeste:hs) = posicao (x-1,y) hs

--45
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (a,b) | y < b = Norte : caminho (x,y+1) (a,b)
                    | y > b = Sul : caminho (x,y-1) (a,b)
                    | x < a = Este : caminho (x+1,y) (a,b)
                    | x > a = Oeste : caminho (x-1,y) (a,b)
                    | otherwise = []

--46
vertical :: [Movimento] -> Bool
vertical [] = False
vertical [Norte] = True
vertical [Sul] = True
vertical (Norte:t) = vertical t
vertical (Sul:t) = vertical t
vertical (Este:t) = False
vertical (Oeste:t) = False

--47
data Posicao = Pos Int Int deriving Show
-- maisCentral :: [Posicao] -> Posicao

--48
-- data Posicao = Pos Int Int deriving Show
vizinhos :: Posicao -> [Posicao] -> [Posicao] 
vizinhos x [] = []
vizinhos x (h:t) | viz1 x h = h:vizinhos x t
                 | otherwise = vizinhos x t

viz1 :: Posicao -> Posicao -> Bool
viz1 (Pos a b) (Pos x y) | a == (x-1) && b == y  = True
                         | a == (x-1) && b == (y-1)  = True
                         | a == (x-1) && b == (y+1)  = True
                         | a == (x+1) && b == y  = True
                         | a == (x+1) && b == (y-1)  = True
                         | a == (x+1) && b == (y+1)  = True
                         | a == x && b == (y-1)  = True
                         | a == x && b == (y+1)  = True
                         | otherwise = False

--49
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = False
mesmaOrdenada [x] = True
mesmaOrdenada [(Pos x y),(Pos a b)] = if(y==b) then True else False
mesmaOrdenada ((Pos x y):(Pos a b):t) | y == b = mesmaOrdenada ((Pos a b):t)
                                      | otherwise = False

--50




