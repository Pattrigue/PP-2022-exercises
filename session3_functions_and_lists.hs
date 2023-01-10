-- Discussion exercises --

onlytwo :: [a] -> Bool
onlytwo [] = False
onlytwo [a] = False
onlytwo [a, b] = True
onlytwo xs = False


{-
For the alldots exercise, you can use list comprehension to extract elements from a list of
tuples using the notation: 
[a | (a, _) <- tupleList]
which extracts the first element of each tuple in tupleList and binds it to the variable a. 
-}
alldots :: Num a => [(a, a)] -> [(a, a)] -> [a]
alldots xs ys = [a * c + b * d |
    (a, _) <- xs,
    (_, b) <- xs,
    (c, _) <- ys,
    (_, d) <- ys]


-- Class exercises --

-- pyt :: (Num a, Enum a, Ord a) => a -> [(a, a, a)]
pyt :: Integral a => a -> [(a, a, a)]
pyt k = [(a, b, c) |
   a <- [1 .. k],
   b <- [1 .. k],
   a <= b,
   c <- [1 .. k],
   b < c,
   a * a + b * b == c * c]


sevens :: Integral a => a -> [a]
sevens k = [x | x <- [1 .. k], x `mod` 7 == 0]


{-
headsup x = if head x == head (tail x) then True else False

It is argued that the type here is [Num] -> Bool, however this is not true.
This is because, in order to compare elements of the list, we need to add a type constraint
so that elements live in Eq. This gives us the following type:
-}

headsup :: Eq a => [a] -> Bool

-- And here is a better solution:
headsup xs = head xs == head (tail xs)


plonk :: Num a => a -> a -> a -> a
plonk x y z = x + y + z


-- We can redefine the curried function plonk in terms of lambda expressions:
-- plonk' :: Num a => a -> a -> a -> a
plonk' :: Num a => a -> a -> a -> a
plonk' = \x -> (\y -> (\z -> (x + y + z)))



-- We need to find a function whose type is:
myFun :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1

-- We know a2 can be compared, and that x and y are of type a.
-- We also know that the return type must be the same type as each element in the tuple.
-- So we can simply compare x and y (satisify Eq a2 constraint) and then return a or b from the tuple.
myFun x y (a, b) = if x == y then a else b


-- Extra exercises -- 

-- Sample input: flop [(1, 'a'),(3, 'r'),(9, 'e')]
flop :: [(a, b)] -> [(b, a)]
flop xs = [(b, a) | (a, b) <- xs]


dupli :: [a] -> [a]
dupli xs = concat (map (\x -> [x, x]) xs)

-- Alternative, shorter definition using concatMap
-- dupli = concatMap (\x -> [x, x])


isperfect :: Integral a => a -> Bool
isperfect n = sum m == n
    where 
        m = [x | x <- [1 .. n - 1], n `mod` x == 0]


bighead :: Ord a => [a] -> Int
bighead xs = length (filter (> head xs) xs)


sums m n = [x + y | x <- [1 .. m], y <- [1 .. n]]


-- Here is an equivalent function that only uses list comprehensions with one generator each:
sums' m n = map (\(x, y) -> x + y) [(x, y) | x <- xs, y <- ys]
    where
        xs = [x | x <- [1 .. m]]
        ys = [y | y <- [1 .. n]]