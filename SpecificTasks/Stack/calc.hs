{- 
#############################################################################
################ DO NOT MODIFY THIS CODE ####################################
#############################################################################
-}

import System.Environment
import System.IO

main :: IO ()
main = do
        args <- getArgs
        let files = get_names args
        let input = fst files 
        let output = snd files
        putStrLn input
        putStrLn output
        in_handle <- openFile input ReadMode
        out_handle <- openFile output WriteMode
        mainloop in_handle out_handle
        hClose in_handle
        hClose out_handle

mainloop :: Handle -> Handle -> IO ()
mainloop in_handle out_handle = 
        do in_eof <- hIsEOF in_handle
           if in_eof
                then return ()
                else do line <- hGetLine in_handle
                        let line_words = words line
                        print $ evaluate line_words
                        hPutStrLn out_handle $ evaluate line_words
                        mainloop in_handle out_handle


get_names :: [String] -> (String, String)
get_names (arg1:arg2:_) = 
        let in_file = arg1
            out_file = arg2
        in (in_file, out_file)
{-
#############################################################################
################ Write your code below this line ############################
#############################################################################
-}


-- Your implementation goes here. Feel free to add more functions.
evaluate :: [String] -> String
evaluate xs = aux xs []

aux :: [String] -> [Int] -> String
aux [] [x] = show x
aux [] stack = "ERROR: Too few operations"
aux (token:t) stack | isOperation token == False = aux t (push stack token)
                    | size < 2 = "ERROR: Too few operands"
                    | token == "+" = aux t ((mysum x y) : drop 2 stack)
                    | token == "-" = aux t ((mysub x y) : drop 2 stack)
                    | token == "*" = aux t ((mymul x y) : drop 2 stack)
                    | token == "/" && y == 0 = "ERROR: Attempted division 0"
                    | token == "/" = aux t ((mydiv x y) : drop 2 stack)
                    | otherwise = "ERROR"
                      where size = length stack
                            (x,y) = get2 stack

mysum x y = x + y
mydiv x y = x `div` y
mymul x y = x * y
mysub x y = x - y

isOperation :: String -> Bool
isOperation s | s == "+" || s == "-" || s == "*" || s == "/" = True
              | otherwise = False

get2 :: [Int] -> (Int,Int)
get2 [] = (-1,-1)
get2 [x] = (-1,-1)
get2 (x:y:t) = (y,x) 

pop :: [Int] -> (Int,[Int])
pop (h:t) = (h,t)

push :: [Int] -> String -> [Int]
push l token = (read token::Int):l

