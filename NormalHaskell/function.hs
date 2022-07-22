-- take a number and return number * 4
quadruple :: Num a => a -> a
quadruple = (*4)


-- a list a recursively compare its first element to its last
-- return true if the first and last element are always equal
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome l | head l == last l = isPalindrome $ take (s-2) (tail l)
               | otherwise = False
                  where s = length l

-- take a list and return 2 lists
-- each list has half of the length of the initial list
halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve l = (take half l,drop half l)
           where size = length l
                 half = size `div` 2 

-- takes a number and returns the max number x -> (10^x <= number)
log10 :: Integral a => a -> a
log10 a | a <= 1 = 1
        | otherwise = auxLog a 1

auxLog :: Integral a => a -> a -> a
auxLog a exp | a < 10^exp = exp - 1
             | otherwise = auxLog a (exp+1)  


-- takes an element and a list
-- case the element isn't in the list return -1
-- otherwise return element index
find ::  Eq a => a -> [a] -> Int
find _ [] = -1
find a l = auxFind a l 0

-- compare the given element to the list elemente
-- return -1 or element index
auxFind :: Eq a => a -> [a] -> Int -> Int
auxFind _ [] _ = -1
auxFind a l index | a == head l = index
                  | otherwise = auxFind a (tail l) (index + 1)

-- takes a number and returns its factorial
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1) 