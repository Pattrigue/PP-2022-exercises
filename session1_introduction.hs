-- Discussion exercise(s) --

second :: [a] -> Maybe a
second [] = Nothing
second [a] = Nothing
second xs = Just (head (tail xs))


-- Class exercises --

allbutsecond :: [a] -> [a]
allbutsecond [] = []
allbutsecond [a] = [a]
allbutsecond (x:y:xs) = x : xs


midtover :: [a] -> ([a], [a])
midtover xs = (take n xs, drop n xs) -- also possible using splitAt
    where n = length xs `div` 2


{-
bingo (x, y) = x mod z
where
z = y + 42

Fixed version below:
-}

bingo (x, y) = x `mod` z
    where
    z = y + 42


last' xs = head (reverse xs) -- in Prelude there already is a function called last


-- qsort in descending order by swapping the <= and >
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = small ++ [x] ++ big
                 where small = qsort [a | a <- xs, a > x]
                       big   = qsort [a | a <- xs, a <= x]