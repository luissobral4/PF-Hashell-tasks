main :: IO ()
main = do s <- mainAux []
          return ()

mainAux :: [Int] -> IO ()
mainAux l =  do putStrLn "Enter a number or a '.'"
                line <- getLine
                if (line == ".") then (putStrLn ("Sum: " ++ show (sum l)))
                else do mainAux (l ++ [read line])