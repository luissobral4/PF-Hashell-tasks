main :: IO ()
main = do s <- aux []
          return ()

aux :: [Int] -> IO ()
aux l =  do putStrLn "Enter a number or a '.'"
            input <- getLine
            if (input == ".") then (putStrLn ("Sum = " ++ show (foldr (+) 0 l)))
            else do aux ((read input) : l)