import System.IO
import System.Directory

copyIo :: String -> String -> IO ()
copyIo f1 f2 = do exists1 <- doesFileExist f1
                  exists2 <- doesFileExist f2
                  if (exists1 == False) then putStrLn "The read file dont't exists"
                  else
                    if (exists2 == True)  then putStrLn "The write file already exists"
                    else do f <- openFile f1 ReadMode
                            c <- hGetContents f
                            writeFile f2 c