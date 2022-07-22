import System.Random
-- randomIO :: Random a => IO a
-- randomRIO :: Random a => (a,a) -> IO a

--b
mastermind :: IO()
mastermind = do x <- geracodigo
                darespostas x 15

geracodigo :: IO [Char]
geracodigo = sequence (replicate 4 (randomRIO ('0','9')))
-- geracodigo = do x <- randomRIO (0,9)
--                 y <- randomRIO (0,9)
--                 w <- randomRIO (0,9)
--                 z <- randomRIO (0,9)
--                 return [x,y,w,z]

darespostas :: [Char] -> Int -> IO()
darespostas s 0 = do putStr "Já foste"
                     return ()
darespostas seg x = do putStr "Jogada?"
                       resp <- getLine
                       let (c,cc) = calcula seg resp
                       putStrLn (show c ++ " nº certos na posição certa")
                       putStrLn (show cc ++ " nº certos na posição errada")
                       if (c == 4) then do putStrLn ("Parabéns\a\a\a\a")
                                           --return ()
                       else do putStrLn "Tente novamente"
                               darespostas seg (x-1)

calcula :: String -> String -> (Int,Int)
calcula gc res = (c gc res, cc)
       where  c [] [] = 0
              c [] a = 0
              c a [] = 0
              c (x:y) (a:b) | x == a = 1 + c y b
                            | otherwise = c y b
              cc = length (interset gc res)

interset ::Eq a=>[a]->[a]->[a]
interset [] l=[]
interset l []=[]
interset (h:s)(x:y) = aux3 (x:y) h ++ interset s (x:y)


aux3 :: Eq a => [a]-> a -> [a]
aux3 [] x = []
aux3 (h:s) x | x== h = [x]
             | otherwise = aux3 s x

-- 2)

data Aposta = Ap [Int] (Int,Int)

--a)
valida :: Aposta -> Bool
valida (Ap [] e) = False
valida (Ap l (x,y)) | (any (\x -> x < 0 || x > 50) l) == False  && x > 0 && x <10 && y > 0 && y < 10 && length l == 5 = True
                    | otherwise = False

-- b)
comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap [] a) (Ap l a2) = (0,contaestrelas a a2)
comuns a1@(Ap (h:t) e) a2@(Ap l e2)   | any (== h) l = (1+c, es)
                                      | otherwise = (c, es)
                                      where (c,es) = comuns (Ap t e) a2

contaestrelas :: (Int,Int) -> (Int,Int) -> Int
contaestrelas (x,y) (a,b) | x == a && y == b = 2
                          | x == a || y == b = 1
                          | otherwise = 0

-- c)
-- i
instance Eq Aposta where
  a == b = comuns a b == (5,2)

-- ii
premio :: Aposta -> Aposta -> Maybe Int
premio a b | a == b = Just 1
           | comuns a b == (5,1) = Just 2
           | comuns a b == (5,0) = Just 3
           | comuns a b == (4,2) = Just 4
           | comuns a b == (4,1) = Just 5
           | comuns a b == (4,0) = Just 6
           | comuns a b == (3,2) = Just 7
           | comuns a b == (2,2) = Just 8
           | comuns a b == (3,1) = Just 9
           | comuns a b == (3,0) = Just 10
           | comuns a b == (1,2) = Just 11
           | comuns a b == (2,1) = Just 12
           | comuns a b == (2,0) = Just 13
           | otherwise = Nothing

-- d)
-- i)
leAposta :: IO Aposta
leAposta = leap

leap :: IO Aposta
leap  = do putStrLn "Insira os 5 números"
           n1 <- getLine
           n2 <- getLine
           n3 <- getLine
           n4 <- getLine
           n5 <- getLine
           putStrLn "Insira as 2 estrelas"
           e1 <- getLine
           e2 <- getLine
           putStrLn ("(Ap [" ++ n1 ++ "," ++ n2 ++ "," ++ n3 ++ "," ++ n4 ++ "," ++ n5 ++ "] (" ++ e1 ++ "," ++ e2 ++"))")
           let aposta = (Ap [read n1,read n2,read n3,read n4,read n5] (read e1,read e2))
           if (valida aposta) then return aposta
           else do (putStrLn ("Aposta Inválida"))
                   leap

joga :: Aposta -> IO ()
joga ap = do x <- leap
             let (Just p) = premio ap x
             
             if (premio ap x == Nothing) then putStrLn "Sem premio"
             else (putStrLn ("Prémio "++ show p))

--e)
geraChave :: IO Aposta
geraChave = do e1 <- randomRIO (0,9)
               e2 <- randomRIO (0,9)
               x1 <- randomRIO (0,50)
               x2 <- randomRIO (0,50)
               x3 <- randomRIO (0,50)
               x4 <- randomRIO (0,50)
               x5 <- randomRIO (0,50)
               let l = [x1,x2,x3,x4,x5]
               return (Ap l (e1,e2))
               -- let l = replicate 5 (randomRIO (0,50))
               -- let n1 = (l !! 0)
               -- let n2 = (l !! 1)
               -- let n3 = (l !! 2)
               -- let n4 = (l !! 3)
               -- let n5 = (l !! 4)
               --putStrLn ("Ap [" ++ show x1 ++ "," ++ show x2 ++ "," ++ show x3 ++ "," ++ show x4 ++ "," ++ show x5 ++ "] (" ++ show e1 ++ "," ++ show e2 ++ ")")

-- f)
main :: IO ()
main = do ch <- geraChave
          ciclo ch

menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
       where menutxt = unlines ["",
                                "Apostar ........... 1",
                                "Gerar nova chave .. 2",
                                "",
                                "Sair .............. 0"]

ciclo :: Aposta -> IO ()
ciclo ap = do m <- menu
              if m == "0" then return ()
              else (if m == "2" then do ch <- geraChave
                                        ciclo ch
                    else do joga ap
                            ciclo ap)
