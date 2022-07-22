import Debug.Trace

-- r will calculate discriminant value then the result is calculated for each case (==0,<0,>0)
-- for each case trace is used to print the discriminant value
quadratic :: Double -> Double -> Double -> [Double]
quadratic a b c | r == 0 = trace ("Null discriminant "++show r) ([-b])
                | r < 0 = trace ("Negative discriminant "++show r) []
                | otherwise = trace ("Positive discriminant "++show r) [-b+r,-b-r]
                  where r = b * b - 4 * a * c 


      