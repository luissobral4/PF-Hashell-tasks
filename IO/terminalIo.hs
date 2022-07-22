main :: IO ()
main = do s <- sum2 0
          return ()

sum2 :: Int -> IO ()
sum2 s =  do putStrLn "Enter a number or a '.'"
             input1 <- getLine
             if (input1 == ".") then (putStrLn ("Sum= " ++ show s))
             else do sum2 (s + read input1)