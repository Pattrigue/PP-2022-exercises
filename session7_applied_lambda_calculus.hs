{-
The lambda calculus formation rules
e ::= x | λx.e1 | e1e2 | let x = e1 in e2 | if e1 then e2 else e3 | letrec f = e1 in e2 | c
can be defined in Haskell as follows:
-}

data Expr = Var String
          | Lam String Expr
          | App Expr Expr
          | Let String Expr Expr
          | If Expr Expr Expr
          | LetRec String Expr Expr
          | Const Const

data Const = Int Int
           | Bool Bool

{-
Here, Expr represents an expression in the lambda calculus, and Const represents a constant value (either an Int or a Bool).

The Var constructor represents a variable, Lam represents a lambda abstraction, App represents function application, Let represents a let binding, If represents an if-then-else expression, LetRec represents a recursive let binding, and Const represents a constant value.

For example, the expression (λx.x) ((λy.y) z) could be represented as App (Lam "x" (Var "x")) (App (Lam "y" (Var "y")) (Var "z")).
-}


triples :: [(a, a, a)] -> ([a], [a], [a])
triples [] = ([], [], [])
triples ((x, y, z):tuples) = (x:xs, y:ys, z:zs)
    where (xs, ys, zs) = triples tuples


wrapup :: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup (x:xs) = elements ++ wrapup nextElements
    where
        elements = [x : takeWhile (==x) xs]
        nextElements = dropWhile (==x) xs


data Tree a = Leaf a | Empty | Node (Tree a) a (Tree a)

insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Leaf x
insert (Leaf a) x
    | a < x     = Node (Leaf a) x Empty
    | otherwise = Node (Leaf x) a Empty
insert (Node l m r) x
    | x < m     = Node (insert l m) m r
    | x > m     = Node l m (insert r x)
    | otherwise = Node l m r
