-- Discussion exercises --
-- Most important higher-order functions: map, filter, foldr, foldl

positions :: Enum a => [a] -> [Int]
positions str = map (\c -> fromEnum c - 96) str


{-
The 0 in foldr f 0 xs is the initial accumulator value.
When foldr is applied to a list, it will apply the function f to the initial accumulator value and the first element of the list, then it will take the result of this operation and apply f to it and the second element of the list, and so on until it reaches the end of the list.
The final result is the accumulation of all of these applications of f.

As an example, if we replace 0 with 10 and call sumsq on 4, the result will be 40 instead of 10.
-}
sumsq :: Int -> Int
sumsq n = foldr (\x y -> (x^2) + y) 0 [1..n]


-- Class exercises --

within xs (min, max) = filter (\n -> n >= min && n <= max) xs



sumrows xs = map sum xs


-- In this exercise, we can simply copy in the factorial defition, then use foldr to apply 1 / k! to each number from 0 to n.
approx n = foldr (\x acc -> (1 / fact x) + acc) 0 [0 .. n]
    where
        fact k = product [1 .. k]
{-
fingo adds each element in ys to the front of xs, starting with the last element of ys and working towards the front.
The final result will be a new list that is the concatenation of xs and ys.
-}
fingo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys

{-
If we swap xs and ys around, we can see that it simply does the opposite.
So      fingo' [1,2,3,4] [5,6,7,8] becomes [1,2,3,4,5,6,7,8]
whereas fingo  [1,2,3,4] [5,6,7,8] becomes [5,6,7,8,1,2,3,4]
-}
fingo' :: [a] -> [a] -> [a]
fingo' xs ys = foldr (:) ys xs

-- This means that fingo' [1,2,3,4] [5,6,7,8] gives the same result as concat [[1,2,3,4], [5,6,7,8]].


singlemap :: (a -> b) -> [a] -> [b]
singlemap = map

-- doublemap :: (a -> (a -> b) -> [a] -> [b]) -> [a] -> [b]
doublemap :: [a -> b] -> [[a] -> [b]]
doublemap = map map


triplemap :: [[a -> b]] -> [[[a] -> [b]]]
triplemap = map (map map)

-- We can see that, since map produces a list, map map will produce a list of lists.


-- Extra exercises --

{-
Here is an implementation of filter using foldr.
For each element in the list, we check the predicate p.
If the predicate is true, we cons the element to the accumulator.
Otherwise, we simply return the accumulator.
Note that the accumulator starts as being the empty list [].
The accumulator will end up containing all elements that satisfy the predicate p.
-}
filter' p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs


-- Returns a new string consisting of characters in the second string not in the first string.
-- Sample input: remove "first" "second"
-- > "econd"
remove :: String -> String -> String
remove first second = foldr appendUnique [] second
    where
        appendUnique char resultStr
            -- if char is in the first input string, return the accumulated result string
            | char `elem` first = resultStr
            -- otherwise, it's unique, so we append the character to the accumulated result string
            | otherwise         = char : resultStr


-- Here is a function that finds the seconds smallest value in a list using foldr and list comprehension
-- The initial accumulator value is just the maximum element of the list
min2 [] = 0
min2 [a] = a
min2 xs
    | length ys >= 2 = head ys
    | otherwise      = foldr (\x min -> if x < min && x > minimum xs then x else min) (maximum xs) xs
    where
        ys = [x | x <- xs, x == minimum xs]