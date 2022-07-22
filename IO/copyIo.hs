import System.IO
import System.Directory

copyIo :: String -> String -> IO ()
copyIo f1 f2 = do file1 <- openFile f1 ReadMode
                  contents <- hGetContents file1
                  b <- doesFileExist f2
                  if b then putStrLn "Second file already exists!"
                  else writeFile f2 contents