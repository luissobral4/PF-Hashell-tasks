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

-- a list of all customers who submitted a rating, without duplicates
customers :: [Rating] -> [Customer]
customers rs = Set.toList ( Set.fromList ( map (\(x,y,z) -> x) rs))

-- a list of all products that have been rated, without duplicates
products :: [Rating] -> [Product]
products rs = Set.toList ( Set.fromList ( map (\(x,y,z) -> y) rs))


-- customers and the number of products they have rated
numScores :: [Rating] -> [(Customer, Int)]
numScores rs = Map.toList (numScores2 Map.empty rs)

numScores2 :: Map Customer Int -> [Rating] -> Map Customer Int
numScores2 m [] = m
numScores2 m rs | Map.member c m = numScores2 (Map.adjust (+1) c m) l
                | otherwise = numScores2 (Map.insert c 1 m) l
                  where (c,p,s) = head rs
                        l = tail rs

-- a list of the customers who give the same score in all their ratings
consistent :: [Rating] -> [Customer]
consistent rs = Map.keys $ Map.filter (\s -> Set.size s == 1) $ Map.map (\l -> Set.fromList l) $ Map.fromListWith (++) $ map (\(c,p,s) -> (c,[s])) rs

-- the average score of each product
averageScore :: [Rating] -> [(Product, Double)]
averageScore rs = Map.toList $ Map.map (\l -> fromIntegral(sum l) / fromIntegral(length l)) $ Map.fromListWith (++) $ map (\(c,p,s) -> (p,[s])) rs


-- the products that have each been rated by every customer
popular :: [Rating] -> [Product]
popular rs = Map.keys (Map.filter (==size) m)
             where m = pMap rs Map.empty
                   size = length (customers rs)

pMap :: [Rating] -> Map Product (Set Customer) -> Map Product (Int)
pMap [] m = Map.map (length) m
pMap rs m | not (Map.member p m) = pMap (tail rs) (Map.insert p (Set.fromList [c]) m)
          | otherwise = pMap (tail rs) (Map.adjust (\st -> Set.insert c st) p m)
            where (c,p,s) = head rs
