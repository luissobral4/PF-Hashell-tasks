--- Ficha 6
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

--a
altura :: BTree a -> Int
altura Empty = 0
altura (Node x l r) = 1 + max (altura l) (altura r)

--b
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node a l r) = 1 + contaNodos l + contaNodos r

--c
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node x Empty Empty) = 1
folhas (Node x l r) = folhas l + folhas r

--d
prune :: Int -> BTree a -> BTree a
prune 0 (Node x l r) = (Node x Empty Empty)
prune x Empty = Empty
prune x (Node n l r) = (Node n (prune (x-1) l) (prune (x-1) r))

--e
path :: [Bool] -> BTree a -> [a]
path [] a = []
path c Empty = []
path (h:t) (Node x l r) | h == True = x : path t r
                        | otherwise = x : path t l

--f
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x l r) = (Node x (mirror r) (mirror l))

--g
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node a l r) (Node b l2 r2) = (Node (f a b) (zipWithBT f l l2) (zipWithBT f r r2))

--h
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) l r) = (Node a x1 y1,Node b x2 y2,Node c x3 y3)
                             where (x1,x2,x3) = unzipBT l
                                   (y1,y2,y3) = unzipBT r

--a
minimo :: Ord a => BTree a -> a
minimo (Node x Empty Empty) = x
minimo (Node x e d) = minimo e

--b
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d) = (Node x (semMinimo e) d)

--c
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (mim,Node x semin d)
         where (mim,semin) = minSmin e

--d
remove :: Ord a => a -> BTree a -> BTree a
remove a Empty = Empty
remove a n@(Node x e d) | a < x = (Node x (remove a e) d)
                        | a > x = (Node x e (remove a d))
                        | otherwise = removeraiz n
removeraiz (Node r Empty d) = d
removeraiz (Node r e Empty) = e
removeraiz (Node r e d) = let m = maior e
                              e1 = semMaior e
                          in  Node m e1 d

maior (Node x e Empty) = x
maior (Node x e d) = maior d

semMaior (Node x e Empty) = e
semMaior (Node x e d) = (Node x e (semMaior d))

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving Show
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por número)

inscNum :: Numero -> Turma -> Bool
inscNum a Empty = False
inscNum n (Node (x,y,z,l) e d) | n == x = True
                               | n > x = inscNum n d
                               | n < x = inscNum n e

inscNome :: Nome -> Turma -> Bool
inscNome a Empty = False
inscNome n (Node (x,y,z,l) e d) | n == y = True
                                | otherwise = inscNome n e || inscNome n d

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n,m,TE,c) e d) = trabEst e ++ [(n,m)] ++ trabEst d
trabEst (Node (n,m,r,c) e d) = trabEst e ++ trabEst d

nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing
nota n (Node (n2,m,r,c) e d) | n == n2 = Just c
                             | n < n2 = nota n e
                             | n > n2 = nota n d

percFaltas :: Turma -> Float
percFaltas t = alunosfaltaram t / fromIntegral (contaNodos t)

alunosfaltaram :: Turma -> Float
alunosfaltaram Empty = 0
alunosfaltaram (Node (a,b,c,Faltou) e d) = 1 + alunosfaltaram e + alunosfaltaram d
alunosfaltaram (Node x e d) = alunosfaltaram e + alunosfaltaram d

mediaAprov :: Turma -> Float
mediaAprov t = somanotas t / alunospassaram t

alunospassaram :: Turma -> Float
alunospassaram Empty = 0
alunospassaram (Node (a,b,c,Aprov x) e d) = 1 + alunospassaram e + alunospassaram d
alunospassaram (Node x e d) = alunospassaram e + alunospassaram d

somanotas :: Turma -> Float
somanotas Empty = 0
somanotas (Node (a,b,c,Aprov x) e d) = 1 + somanotas e + somanotas d
somanotas (Node x e d) = somanotas e + somanotas d

aprovAv :: Turma -> Float
aprovAv t = (alunospassaram t) / (alunospassaram t + alunosavaliados t)

alunosavaliados :: Turma -> Float
alunosavaliados Empty = 0
alunosavaliados (Node (a,b,c,Rep) e d) = 1 + alunosavaliados e + alunosavaliados d
alunosavaliados (Node x e d) = alunosavaliados e + alunosavaliados d
