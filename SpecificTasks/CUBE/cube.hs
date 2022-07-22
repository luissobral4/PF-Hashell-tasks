import Data.List

data Point = Point Int Int Int
data Cube = Cube {start::Point, size::Int, color::Char }
type Result = [String]

sampleInput :: [Cube]
sampleInput = [Cube {start = Point 1 1 10, size = 4, color = 'X'},
               Cube {start = Point 1 5 10, size = 3, color = 'O'},
               Cube {start = Point 10 8 0, size = 2, color = '#'},
               Cube {start = Point 0 0 0, size = 10, color = '*'}]


main :: IO ()
main = pp $ view sampleInput

pp :: Result -> IO()
pp r = if r == [] then return()
       else do putStrLn (head r)
               pp (tail r)

view :: [Cube] -> Result
view l = reverse $ foldr rComp [] $ map snd $ sort $ map drawCube l


drawCube :: Cube -> (Int,Result)
drawCube (Cube (Point x y z) 
               size 
               color) = (z+size,(replicate y $ replicate line ' ')++(replicate size $ cubeLine))
                        where line = x + size
                              cubeLine = replicate x ' ' ++ replicate size color


rComp :: Result -> Result -> Result
rComp [] r = r
rComp r [] = r
rComp (f:r1) (f2:r2) = (lineComp f f2) : rComp r1 r2

lineComp :: String -> String -> String
lineComp "" s = s
lineComp s "" = s
lineComp (f:t) (f2:t2) | f2 == ' ' = f:lineComp t t2
                       | otherwise = f2:lineComp t t2
