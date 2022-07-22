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
customers rs = singleValue [cr | (cr, prt, sr) <- rs]

-- a list of all products that have been rated, without duplicates
products :: [Rating] -> [Product]
products rs = singleValue [prt | (cr, prt, sr) <- rs]

-- customers and the number of products they have rated
numScores :: [Rating] -> [(Customer, Int)]
numScores rs = let l = [(cr,prt) | (cr, prt, sr) <- rs]
               in Map.assocs $ Map.map (length . singleValue) $ group l

-- a list of the customers who give the same score in all their ratings
consistent :: [Rating] -> [Customer]
consistent rs = let l = [(cr,sr) | (cr, prt, sr) <- rs]
                in Map.keys $ Map.filter (==1) $ Map.map (length . singleValue) $ group l

-- the average score of each product
averageScore :: [Rating] -> [(Product, Double)]
averageScore rs = let l = [(prt,sr) | (cr, prt, sr) <- rs]
                      average x = fromIntegral(sum x) / fromIntegral(length x)
                  in Map.assocs $ Map.map (average) $ group l

-- the products that have each been rated by every customer
popular :: [Rating] -> [Product]
popular rs = let l = [(prt,cr) | (cr, prt, sr) <- rs]
                 sizeCr = length $ customers rs
                 in Map.keys $ Map.filter (==sizeCr) $ Map.map (length . singleValue) $ group l

singleValue :: Ord x => [x] -> [x]
singleValue = Set.toList . Set.fromList

group l = Map.fromListWith (++) [(k, [v]) | (k, v) <- l]