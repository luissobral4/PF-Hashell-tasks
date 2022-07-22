module Ratings where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

type Customer = String
type Product = String
type Score = Int
type Rating = (Customer, Product, Score)

customers :: [Rating] -> [Customer]
customers rs = removeDup $ 
               map (\(c,p,s) -> c) rs

products :: [Rating] -> [Product]
products rs = removeDup $ 
              map (\(c,p,s) -> p) rs

numScores :: [Rating] -> [(Customer, Int)]
numScores rs = Map.assocs $ 
               Map.map length $ 
               listTomap $ 
               map (\(c,p,s) -> (c,[p])) rs

consistent :: [Rating] -> [Customer]
consistent rs = Map.keys $ 
                Map.filter (\x -> length x == 1) $ 
                listTomap $ 
                map (\(c,p,s) -> (c,[s])) rs

averageScore :: [Rating] -> [(Product, Double)]
averageScore rs = map (\(p,list) -> (p,averageList list)) $ 
                  Map.assocs $
                  listTomap $ 
                  map (\(c,p,s) -> (p,[s])) rs

popular :: [Rating] -> [Product]
popular rs = Map.keys $ 
             Map.filter (\x -> length x == customersSize rs) $ 
             listTomap $ 
             map (\(c,p,s) -> (p,[c])) rs


removeDup ::  Ord a => [a] -> [a]
removeDup l = Set.toList $ 
              Set.fromList l

listTomap :: (Ord a, Ord b) => [(a,[b])] -> Map a [b]
listTomap l = Map.map removeDup $ 
              Map.fromListWith (++) l

customersSize :: [Rating] -> Int
customersSize = length . customers

averageList :: [Int] -> Double
averageList list = fromIntegral(sum list) / fromIntegral(length list)
