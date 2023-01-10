nsonly n = nsonly' n 0
    where
        nsonly' n x = x * n : nsonly' n (x + 1)


-- Equivalent definition without using recursion:
nsonly2 n = [n * x | x <- [0..n + 1]]


-- The second pattern match is the same as indflet _ (x:[]) = ...
-- Calling head (indflet 1 (2:undefined)) causes an exception.
-- (Because of syntax sugar, we try to evaluate whether undefined == [] which throws an exception)
indflet :: a -> [a] -> [a]
indflet _ [] = []
indflet _ [x] = [x]
indflet e (x:y:ys) = x : e : indflet e (y:ys)


-- Here is a fixed version:
fletind :: a -> [a] -> [a]
fletind _ [] = []
fletind e (x:xs) = x : e : fletind e xs


allBinaries :: [String]
allBinaries = ["0", "1"] ++ [y : x | x <- allBinaries, last x /= '0', y <- ['0', '1']]


data Tree = Node Tree Tree | Leaf
data Direction = L | R deriving Show
type Path = [Direction]

allFinitePaths :: Tree -> [Path]
allFinitePaths t = go t []
    where go (Leaf) path = [path]
          go (Node l r) path = go l (path ++ [L]) ++ go r (path ++ [R])

-- Define the tree
--        x
--      /   \
--     x     x
--    / \   / \
--   x   x x   x

sampleTree = Node (Node (Node (Leaf) (Leaf)) (Node (Leaf) (Leaf))) (Node (Node (Leaf) (Leaf)) (Node (Leaf) (Leaf)))

-- Call this function to print the paths
printPaths = do print (allFinitePaths sampleTree)

-- printPaths
-- > [[L,L,L], [L,L,R], [L,R,L], [L,R,R], [R,L,L], [R,L,R], [R,R,L], [R,R,R]]


hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming) (merge (map (3*) hamming) (map (5*) hamming))
    where merge xs [] = xs
          merge [] ys = ys
          merge (x:xs) (y:ys)
              | x < y = x : merge xs (y:ys)
              | x > y = y : merge (x:xs) ys
              | otherwise = x : merge xs ys
