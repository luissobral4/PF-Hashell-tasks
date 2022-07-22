import Debug.Trace

-- r will calculate discriminant value then the result is calculated for each case (==0,<0,>0)
-- for each case trace is used to print the discriminant value
quadratic :: Double -> Double -> Double -> [Double]
quadratic a b c | r == 0 = trace ("Null discriminant "++show r) ([-b])
                | r < 0 = trace ("Negative discriminant "++show r) []
                | otherwise = trace ("Positive discriminant "++show r) [-b+r,-b-r]
                  where r = b * b - 4 * a * c 

-- this function takes number n and calculate divisors of n using aux function
divisor :: Integer -> [Integer]
divisor n = aux n n

-- this function takes the number n and a counter starting at n. Recursively calculate divisors of n comparing all numbers from n to 1
aux :: Integer -> Integer -> [Integer]
aux n 1 = [1]
aux n c | mod n c == 0 = c : aux n (c - 1) 
        | otherwise = aux n (c - 1) 

-- this funtion receives number n as argument
-- divisors value will be a list of the divisors of n calculated by divisor function
-- for each case (prime or not prime) trace is used to print the result
prime n | length divisors == 2 = trace ("The positive integer " ++ show n ++ " IS Prime\nDivisors: " ++ show divisors) divisors
        | otherwise = trace ("The positive integer " ++ show n ++ " is NOT Prime\nDivisors: " ++ show divisors) divisors
          where divisors = (divisor n)