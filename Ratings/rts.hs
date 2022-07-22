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
customers rs = Set.toList $ Set.fromList $ map getCustomer rs 

-- a list of all products that have been rated, without duplicates
products :: [Rating] -> [Product]
products rs =  Set.toList $ Set.fromList $ map getProduct rs

-- customers and the number of products they have rated
numScores :: [Rating] -> [(Customer, Int)]
numScores rs =  Map.toList $ Map.fromListWith (+) $ map (\e -> (getCustomer e,n)) rs

-- a list of the customers who give the same score in all their ratings
consistent :: [Rating] -> [Customer]
consistent rs = Map.keys $ Map.filter (f1) $ Map.map (f2) $ Map.fromListWith (++) $ map (\e -> (getCustomer e,[getScore e])) rs
                where f1 s = Set.size s == 1
                      f2 s = Set.fromList s

-- the average score of each product
averageScore :: [Rating] -> [(Product, Double)]
averageScore rs = Map.assocs $ Map.map (f) $ Map.fromListWith (++) $ map (\e -> (getProduct e,[getScore e])) rs
                  where f l = fromIntegral(sum l) / fromIntegral(length l)

-- the products that have each been rated by every customer
popular :: [Rating] -> [Product]
popular rs = Map.keys $ Map.filter (f1) $ Map.map (f2) $ Map.fromListWith (++) $ map (\e -> (getProduct e,[getCustomer e])) rs
             where size = length $ customers rs
                   f1 s = Set.size s == size
                   f2 s = Set.fromList s


getCustomer :: Rating -> Customer
getCustomer (c,p,s) = c

getProduct :: Rating -> Product
getProduct (c,p,s) = p

getScore :: Rating -> Score
getScore (c,p,s) = s

n :: Int
n = 1
