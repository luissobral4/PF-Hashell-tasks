module Ratings where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type Customer = String
type Product = String
type Score = Int

-- a single rating of a product by a customer
type Rating = (Customer, Product, Score)

r1 :: [Rating]
r1 = [("c1","p1",1),("c1","p2",2),("c2","p1",4),("c3","p3",8),("c3","p1",5),("c4","p1",5),("c1","p3",10),("c2","p3",4)]

-- a list of all customers who submitted a rating, without duplicates
customers :: [Rating] -> [Customer]
customers rs = Set.toList s
               where s = Set.fromList $ map (\(c,p,s) -> c) rs 

-- a list of all products that have been rated, without duplicates
products :: [Rating] -> [Product]
products rs = Set.toList s
              where s = Set.fromList $ map (\(c,p,s) -> p) rs 

-- customers and the number of products they have rated
numScores :: [Rating] -> [(Customer, Int)]
numScores rs = scoreMap rs Map.empty

scoreMap :: [Rating] -> Map Customer Int -> [(Customer, Int)]
scoreMap [] m = Map.toList m
scoreMap ((c,p,s):rs) map | Map.member c map == False = scoreMap rs (Map.insert c 1 map)
                          | otherwise = scoreMap rs (Map.adjust (+1) c map)


-- a list of the customers who give the same score in all their ratings
consistent :: [Rating] -> [Customer]
consistent rs = consistentMap rs Map.empty

consistentMap :: [Rating] -> Map Customer (Set Int) -> [Customer]
consistentMap [] m = Map.keys (Map.filter (\x -> length x == 1) m)
consistentMap ((c,p,s):rs) map | Map.member c map == False = consistentMap rs (Map.insert c (Set.singleton s) map)
                               | otherwise = consistentMap rs (Map.adjust (\set -> Set.insert s set) c map)

-- the average score of each product
averageScore :: [Rating] -> [(Product, Double)]
averageScore rs = averageMap rs Map.empty

averageMap :: [Rating] -> Map Product [Int] -> [(Product, Double)]
averageMap [] m = map (\(p,l) -> (p,fromIntegral (sum l) / fromIntegral (length l))) (Map.toList m)
averageMap ((c,p,s):rs) map | Map.member p map == False = averageMap rs (Map.insert p [s] map)
                            | otherwise = averageMap rs (Map.adjust (\l -> s:l) p map)

-- the products that have each been rated by every customer
popular :: [Rating] -> [Product]
popular rs = popularMap rs (length $ customers rs) Map.empty

popularMap :: [Rating] -> Int -> Map Product (Set Customer) -> [Product]
popularMap [] n map = Map.keys (Map.filter (\x -> length x == n) map)
popularMap ((c,p,s):rs) n map | Map.member p map == False = popularMap rs n (Map.insert p (Set.singleton c) map)
                              | otherwise = popularMap rs n (Map.adjust (\set -> Set.insert c set) p map)
                           