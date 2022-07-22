module Quiz2 where

{- QUIZ 2 INSTRUCTIONS

All questions have equal weight and all require Haskell code answers. There are two bonus questions.  For all questions, give definitions for the currently undefined functions.  You cannot change the type of the functions.

There are two parts to the quiz.

In the first part you can use anything that's present when ghci is started, i.e. the basic language plus the Prelude, EXCEPT LIST COMPREHENSION.  You can use "helper" functions as well.

The second part adds some restrictions, detailed below.

TO SUBMIT: put your answers in this file and attach it to your Brightspace answer. You can submit as many times as you like.  We'll grade the most recent one that is not late.  Please try to comment out broken code, but we'll accept files that don't load.
-}

-- Identifiers
data Id = Id [Char] deriving (Show, Eq)

-- Sample identifiers
p = Id "p"
q = Id "q"
r = Id "r"
s = Id "s"

{-
Exp: a type of boolean expressions (ie. expressions whose variables can only have values true and false) made from "and", "or", "not" and variables.
-}
data Exp = Var Id | Not Exp | And Exp Exp | Or Exp Exp
            deriving (Show, Eq)

anExp = 
    Not 
    (And (And (Or (Var p) (Var q))
              (Not (Var q)))
         (Not (And (Not (Var q)) 
                   (Not (Not (Var r))))))


-- Valuations are mappings associating truth values to identifiers.
-- Valuations are always defined on all Ids. The default value is False.
data Valuation = Val (Id -> Bool)

applyValuation :: Valuation -> Id -> Bool
applyValuation (Val f) x = f x

-- Example list representations of a valuation
aValRep = [(p,True), (q,False), (r,True)]
aValRep' = [(p,True), (q,False), (r,False)]

{- Q1
Given a list of pairs [(x1,b1), ..., (xn,bn)] construct a valuation f where
f x = bi if x=xi for some i, 1 <= i <= n, and
f x = False otherwise
>>> applyValuation (makeValuation aValRep) r
True
-}
makeValuation :: [(Id, Bool)] -> Valuation
makeValuation = undefined

{- Q2
hasVar x e = True iff the variable named x occurs somewhere in e
>>> hasVar p anExp
True
-}
hasVar :: Id -> Exp -> Bool
hasVar (Id i) (Var (Id v)) = i == v
hasVar i (Not e) = (hasVar i e)
hasVar i (And e1 e2) = (hasVar i e1) || (hasVar i e2)
hasVar i (Or e1 e2) = (hasVar i e1) || (hasVar i e2) 

{- Q3
A *subterm* of an expression e is any expression that can be reached by starting at the root (thinking of e as a tree) and going down through constructors.  E.g. the subterms of "(x and y) or z" are: "(x and y) or z", "x and y", "x", "y" and "z".
anySubterm p e = True iff p is true of some subterm of e.
>>> anySubterm ((== (Not (Var r)))) anExp
True
-}
anySubterm :: (Exp -> Bool) -> Exp -> Bool
anySubterm f e@(Var i) = f e
anySubterm f e@(Not e1) = (f e) || (anySubterm f e1)
anySubterm f e@(And e1 e2) = (f e) || (anySubterm f e1) || (anySubterm f e2)
anySubterm f e@(Or e1 e2) = (f e) || (anySubterm f e1) || (anySubterm f e2)

{- Q4
(evalExp v e) is the boolean value of e when the variables in e have the values given by v.
>>> evalExp aVal anExp
True
>>> evalExp aVal' anExp
-}
evalExp :: Valuation -> Exp -> Bool
evalExp val (Var x) = applyValuation val x
evalExp val (Not e1) = evalExp val e1
evalExp val (And e1 e2) = evalExp val e1 || evalExp val e2
evalExp val (Or e1 e2) = evalExp val e1 || evalExp val e2



{- Q Bonus 1
A "literal" is a boolean expression of the form "x" or "not x" for x a variable.
A "clause" has the form "l1 or l2 or ... or lm", for m>=1, where each li is a literal.
A CNF formula has the form "c1 and c2 and ... and cn" for n>=1 where each ci is a clause.

(norm e) is an expression logically equivalent to e where all occurrences of Not are applied to a variable. Saying e is "Logically equivalent" to e' means that (eval v e) = (eval v e') for all v.
Hint: recall the following logical equivalences for boolean expressions:
not (not e) *is equivalent to* e
not (e and e') *is equivalent to* (not e) or (not e')
not (e or e') *is equivalent to* (not e) and (not e')
You can used these equivalences to push all the negations (Not) down through Ands and Ors.
>>> norm anExp 
Or 
(Or (Or (Not (Var (Id "p")))
        (Not (Var (Id "q")))) 
    (Var (Id "q"))) 
(And (Not (Var (Id "q"))) 
     (Var (Id "r")))
-}
norm :: Exp -> Exp
norm (Var i) = Var i
norm (Not (Var v)) = (Not (Var v))
norm (Not (Not e)) = norm e
norm (Not (And e1 e2)) = (Or (norm (Not e1)) (norm (Not e2)))
norm (Not (Or e1 e2)) = (And (norm (Not e1)) (norm (Not e2)))
norm (And e1 e2) = (And (norm e1) (norm e2))
norm (Or e1 e2) = (Or (norm e1) (norm e2))

{-
RESTRICTIONS for the remaining questions.
- NO RECURSIVE DEFINITIONS except for the function in the question (ie. if you are writing a function add you can call add recursively, but not any other function)
- NO LIST COMPREHENSION

You CAN still continue to use anything from the Prelude, but you don't need to know any predefined functions except for, possibly, map, fold, filter, {m..n} and zip.   You can use "helper" functions too, as long as their definitions are not recursive.  
-}

{- 
A type of "wide" trees (a node can have an unbounded number of children) with integer keys in the nodes.
A *leaf* is a node with no children, i.e. of the form WT k [].
-}
data WTree = WT Int [WTree] deriving Show

leaf :: Int -> WTree
leaf n = WT n []

aWT :: WTree
aWT = WT 3 [leaf 4, WT 17 [leaf 7, leaf 8, leaf 9], leaf 11]

{- Q5
No recursive definitions except for mapLeaf.
(mapLeaf f e) applies function f to the key of every leaf of e, returning the modified tree.
>>> mapLeaf (+1) aWT
WT 3 [WT 5 [],WT 17 [WT 8 [],WT 9 [],WT 10 []],WT 12 []]
= WT 3 [leaf 5, WT 17 [leaf 8, leaf 9, leaf 10], leaf 12])
-}
mapLeaf :: (Int -> Int) -> WTree -> WTree
mapLeaf f (WT n []) = (WT (f n) [])
mapLeaf f (WT v l) = WT v (map (mapLeaf f) l)


{- Q6
No recursive definitions except for maxFanout.
The *fanout* of a subtree/node (WT k l) is the length of l. The function maxFanout finds the largest fanout in the given tree (including the root node)
>>> maxFanout aWT 
3
-}
maxFanout :: WTree -> Int
maxFanout (WT n []) = 1
maxFanout (WT v l)  = max (length l) (maximum (map (maxFanout) l))

{- Q7
No recursive definitions except for leafKeyList
(leafKeyList t) computes the list of keys appearing in the leaves of the given tree, in left-to-right order.
>>> leafKeyList aWT
[4,7,8,9,11]
-}
leafKeyList :: WTree -> [Int]
leafKeyList (WT n []) = [n]
leafKeyList (WT v l) = concat (map (leafKeyList) l)

{- Q8
No recursive definitions except for equalWT
(equalWT e e') = True iff e and e' are identical.
>>> equalWT aWT aWT
True
>>> equalWT aWT (mapLeaf (1+) aWT)
False
Hint: one way to do this is using zip :: [a] -> [b] -> [(a,b)].
-}
equalWT :: WTree -> WTree -> Bool
equalWT (WT n []) (WT n2 []) = True
equalWT (WT v1 l1) (WT n []) = False 
equalWT (WT n []) (WT v1 l1) = False
equalWT (WT n l2) (WT v1 l1) = (length l2 == length l1) && and (map (uncurry $ equalWT) (zip l1 l2))

{- Q Bonus 2
No recursive definitions except for listifyTransitions
The input is a square table of booleans represented as a list of rows.  If the element in row i and column j is True we say there is a *transition* from i to j. The function listifyTransitions gives a list of rows where the i^th row is the list of all j such that there is a transition from i to j.
>>> listifyTransitions transitions
[[1,3],[0,1],[],[0,1,2,3]]
-}
listifyTransitions :: [[Bool]] -> [[Int]]
listifyTransitions = undefined

transitions :: [[Bool]]
transitions =
    let f = False
        t = True
    in
    [[f,t,f,t],
     [t,t,f,f],
     [f,f,f,f],
     [t,t,t,t]]
