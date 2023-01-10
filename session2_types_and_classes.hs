-- Discussion exercise(s) --

quango :: a -> [a]
quango x = replicate 4 x
-- quango is parametric polymorphic because a can be any type
-- and there is no type constraint anywhere

tango :: Num p1 => (a, b) -> p2 -> p1
tango (x, y) z = 69
-- tango is both parametric polymorphic and ad-hoc polymorphic
-- parametric polymorphism because there is no type class constraint on a, b, and p2
-- ad-hoc polymorphic because there is a type class constraint on p1


-- Class exercises --

-- twice is a function that takes a function (denoted (a -> a) and a value x (denoted -> a))
-- and returns the result of applying f on x twice (denoted with the final -> a)
-- twice is parametric polymorphic because there is no type class constraint on a
twice :: (a -> a) -> a -> a
twice f x = f (f (x))


-- dingo just takes a tuple and returns a list consisting of the same type as the tuple elements
-- It is parametric polymorphic because there is no type class constraint on the variables
dingo :: (a, a) -> [a]
dingo (x, y) = [x, y]


-- The type of bighead is the following:
bighead :: Ord a => [a] -> Int

{-
This is because, due to the nature of how the function works, we need to impose a constraint
on the list type since we need to be able to compare elements in the list.

So, the list must consist of elements that are members of the type class Ord, because that
allows us to compare elements in the list.

Finally, we return an Int, not a Num, because Num includes ALL numeraic types including Float,
Double, Int, etc., however we are interested in the number of elements greater than the head,
so we are interested in an integer.

Here is an example of how bighead could be implemented:
-}
bighead xs = length (filter (> head xs) xs)


mango :: Num a => a -> a -> a -> a
mango x y z = x * y + z - 42
-- mango is only ad-hoc polymorphic because it imposes a type class constraint on a


bingo :: a -> a
bingo x = head (replicate 10 x)
-- bingo is only parametric polymorphic because it does not impose any type constraint on a, 
-- and no other variables are involved.