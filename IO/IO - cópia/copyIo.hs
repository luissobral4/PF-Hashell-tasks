import System.IO
import System.Directory

copyIo :: String -> String -> IO ()
copyIo file1 file2 = do f <- openFile file1 ReadMode
                        contents <- hGetContents f
                        boolean <- doesFileExist file2
                        if (boolean == True) then putStrLn "The destiny file already exists"
                        else writeFile file2 contents