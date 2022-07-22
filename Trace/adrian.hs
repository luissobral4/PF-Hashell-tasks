import Debug.Trace

--secondOrder 1 2 1
--secondOrder 1 3 2
--secondOrder 5 3 2

-- discriminant value is calculated whih the a,b,c values then the result is calculated for each case (0,<0,>0)
-- trace is used to print the discriminant value for each one of the three cases
secondOrder :: Double -> Double -> Double -> [Double]
secondOrder a b c | discriminant == 0 = trace ("Discriminant "++show discriminant++" is Null") ([-b])
                  | discriminant < 0 = trace ("Discriminant "++show discriminant++" is Negative") []
                  | otherwise = trace ("Discriminant "++show discriminant++" is Positive") [-b-discriminant,-b+discriminant]
                    where discriminant = b * b - 4 * a * c 


      