-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Assessed2 (applyfuns , updateNodes , graft , elimImplications ,
                  isInCNF , toCNF , binToRose , roseToBin) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Exercise 1 -}
applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns f g (Leaf l) = Leaf (g l)
applyfuns f g (Fork l n r) = Fork (applyfuns f g l) (f n) (applyfuns f g r)

{- Exercise 2 -}
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes _ _ Empty = Empty
updateNodes [] f (Node l n r) = Node l (f n) r
updateNodes (GoLeft:t) f (Node l n r) = Node (updateNodes t f l) (f n) r
updateNodes (GoRight:t) f (Node l n r) = Node l (f n) (updateNodes t f r)

{- Exercise 3 -}

graft :: Rose -> [Rose] -> (Rose,[Rose])
graft x [] = (x,[])
graft (Br []) (h:t) = (h,t)
graft (Br x) lst = foldl add (Br [], lst) x


add :: (Rose,[Rose]) -> Rose -> (Rose,[Rose])
add (Br a,lst) x = (Br (a ++ [n]), res)
  where (n,res) = graft x lst


{- Exercise 4 -}

elimImplications :: Expr -> Expr
elimImplications (Var s) = Var s
elimImplications (Not e) = Not (elimImplications e)
elimImplications (Conj x y) = Conj (elimImplications x) (elimImplications y)
elimImplications (Disj x y) = Disj (elimImplications x) (elimImplications y)
elimImplications (Implies x y) = Disj (Not (elimImplications x)) (elimImplications y)


isInCNF :: Expr -> Bool
isInCNF (Conj c@(Conj x y) c2@(Conj x2 y2)) = isInCNF c  && isInCNF c2
isInCNF (Conj c@(Conj x y) (Disj x2 y2)) = isInCNF c && auxIsCNF y2 && auxIsCNF x2
isInCNF (Conj (Disj x2 y2) c@(Conj x y)) = isInCNF c && auxIsCNF y2 && auxIsCNF x2

isInCNF (Conj c@(Conj x y) y2) = isInCNF c && auxIsCNF y2
isInCNF (Conj x2 c@(Conj x y)) = isInCNF c && auxIsCNF x2

isInCNF (Conj (Disj x y) (Disj x2 y2)) = auxIsCNF x && auxIsCNF y && auxIsCNF x2 && auxIsCNF y2
isInCNF (Conj (Disj x y) y2) = auxIsCNF x && auxIsCNF y &&  auxIsCNF y2
isInCNF (Conj x2 (Disj x y)) = auxIsCNF x && auxIsCNF y &&  auxIsCNF x2

isInCNF (Conj x y) = auxIsCNF x && auxIsCNF y
isInCNF (Disj x y) = False
isInCNF e = auxIsCNF e

auxIsCNF :: Expr -> Bool
auxIsCNF (Var x) = True
auxIsCNF (Not (Var x)) = True
auxIsCNF (Not e) = False
auxIsCNF (Implies x y) = False
auxIsCNF (Conj x y) = False
auxIsCNF (Disj x y) = False

remNegPushDisj :: Expr -> Expr
remNegPushDisj (Var x) = (Var x)
remNegPushDisj (Not (Var x)) = (Not (Var x))
remNegPushDisj (Not (Not (Var x))) = (Var x)
remNegPushDisj (Not (Conj e1 e2)) = Disj (remNegPushDisj (Not e1)) (remNegPushDisj (Not e2))
remNegPushDisj (Not (Disj e1 e2)) = Conj (remNegPushDisj (Not e1)) (remNegPushDisj (Not e2))
remNegPushDisj (Not (e)) = (Not (remNegPushDisj e))

remNegPushDisj (Disj (Conj e1 e2) e3) = Conj (Disj (remNegPushDisj e1) (remNegPushDisj e3)) (Disj (remNegPushDisj e2) (remNegPushDisj e3))
remNegPushDisj (Disj e3 (Conj e1 e2)) = Conj (Disj (remNegPushDisj e3) (remNegPushDisj e1)) (Disj (remNegPushDisj e3) (remNegPushDisj e2))
remNegPushDisj (Disj x y) = Disj (remNegPushDisj x) (remNegPushDisj y)
remNegPushDisj (Conj x y) = Conj (remNegPushDisj x) (remNegPushDisj y)
remNegPushDisj e = e 

toCNF :: Expr -> Expr
toCNF e = auxToCNF (elimImplications e)

auxToCNF :: Expr -> Expr
auxToCNF e = if isInCNF e then e else auxToCNF (remNegPushDisj e)

{- Exercise 5 -}
{-
binToRose :: Bin -> Rose
binToRose Root = (Br [])
binToRose (Branch l r) = Br ([binToRose l] ++ [binToRose r])

roseToBin :: Rose -> Bin
roseToBin (Br []) = Root
roseToBin (Br [l,r]) = Branch (roseToBin l) Root
roseToBin (Br (l:r:t)) = Branch (roseToBin l) (roseToBin r)
-}

binToRose :: Bin -> Rose
binToRose b = Br (binToRose2 b)

binToRose2 :: Bin -> [Rose]
binToRose2 Root = []
binToRose2 (Branch l r) = (Br (binToRose2 l)) : binToRose2 r

roseToBin :: Rose -> Bin
roseToBin (Br l) = roseToBin2 l

roseToBin2 :: [Rose] -> Bin
roseToBin2 ([]) = Root
roseToBin2 ((Br h):t) = Branch (roseToBin2 h) (roseToBin2 (t))



