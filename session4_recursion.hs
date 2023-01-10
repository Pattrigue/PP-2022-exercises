-- Discussion exercises --
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' 1 x = [x]
replicate' n x = x : replicate' (n - 1) x


improve :: [a] -> [a]
improve [] = []
improve [x] = [x]
improve (x:xs) = x : improve (tail xs)


-- Class exercises --

rev :: [a] -> [a]
rev [] = []
rev [x] = [x]
rev (x:xs) = rev xs ++ [x]


mylast :: [a] -> a
mylast [] = error "Empty list!"
mylast [x] = x
mylast (x:xs) = mylast xs


wrapup :: Eq a => [a] -> [[a]]
wrapup [] = []
wrapup [x] = [[x]]
wrapup (x:xs) = items : wrapup (drop (length items - 1) xs)
    where
        items = x : take (index 0) xs
        index i
            | xs !! i == x = index (i + 1)
            | otherwise    = i

-- Alternative one-liner using takeWhile and dropWhile
wrapup' [] = [[]]
wrapup' (x:xs) = (x:takeWhile (==x) xs) : wrapup (dropWhile (==x) xs)


-- For this exercise, we can use a similar approach to the one from the last exercise
rle :: Eq a => [a] -> [(a, Int)]
rle [] = []
rle (x:xs) = (x, length (x : takeWhile (==x) xs)) : rle (dropWhile (==x) xs)


{-
This function recursively processes the list of triples, adding each element of the triple to the corresponding list in the output triple. 
The base case of the recursion is when the input list is empty, in which case the function returns an empty triple of lists.
-}
triples :: [(a1, a2, a3)] -> ([a1], [a2], [a3])
triples [] = ([], [], [])
triples ((x, y, z):xs) = (x:xs1, y:xs2, z:xs3)
    where (xs1, xs2, xs3) = triples xs


-- Extra exercises --

isolate :: Eq a => [a] -> a -> ([a], [a])
isolate l x = (l1, l2)
    where
        l1 = [a | a <- l, a /= x]
        l2 = [b | b <- l, b == x]

-- Here is an alternative recursive definition of isolate:
isolate' :: Eq a => [a] -> a -> ([a], [a])
isolate' [] _ = ([], [])
isolate' (l:ls) x
    | l == x    = (l1, l:l2)
    | otherwise = (l:l1, l2)
    where (l1, l2) = isolate' ls x


amy :: (a -> Bool) -> [a] -> Bool
amy f [] = False
amy f (x:xs)
    | f x       = True
    | otherwise = amy f xs


frequencies :: String -> [(Char, Int)]
frequencies [] = []
frequencies (x:xs) = (x, length (x : filter (== x) xs)) : frequencies (filter (/= x) xs)


-- This is a solution I found, as I was unable to solve this exercise myself.
cfrac :: (RealFrac a, Integral b) => a -> b -> [b]
cfrac r n = cfrac' r n []
  where
    cfrac' :: (RealFrac a, Integral b) => a -> b -> [b] -> [b]
    cfrac' r n acc
      | n == 0    = acc
      | otherwise = cfrac' r (n - 1) (floor r : acc)
