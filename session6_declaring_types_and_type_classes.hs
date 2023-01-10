-- Discussion exercises --
data Unary = I Unary | Z

unaryValue :: Unary
unaryValue = I (I (I (I Z)))

-- unaryValue represents IIIIZ, which should be 4 when converted to an integer.

unary2int :: Unary -> Integer
unary2int Z = 0
unary2int (I unary) = 1 + unary2int unary


data Tree a = Leaf a | Node (Tree a) a (Tree a)

mytree = Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))

{-
mytree represents:
  2
 / \
1   4
   / \
  3   5

Here is a function that finds the smallest element in the tree by recursively finding the smallest node in the
left and right children of the root.
-}

least :: Ord a => Tree a -> a
least (Leaf a) = a
least (Node l m r) = minimum [least l, m, least r]


-- Class exercises --

data Aexp = Number Int | Variable String | Add Aexp Aexp | Mult Aexp Aexp

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

assignments :: [([Char], Int)]
assignments = [("x", 3), ("y", 4)]

expr :: Aexp
expr = Add (Variable "x") (Variable "y")

eval :: Aexp -> Assoc String Int -> Int
eval (Number n) _ = n
eval (Variable x) t = find x t
eval (Add e1 e2) t = eval e1 t + eval e2 t
eval (Mult e1 e2) t = eval e1 t * eval e2 t

-- Sample call: eval expr assignments
-- > 7


{-
Here is the bad definition of Unix directories from the exercise:

data Dir a String b Int = Empty Null | Mult Dir Dir | Subdir Dir

There are several issues with this definition:

- The a and b type variables are not being used. It looks like they intended to use them to represent the type of the name and size respectively, but they have not used them in the rest of the definition.

- The Empty and Null constructors do not make sense. It looks like they intended to use Empty to represent an empty directory, but they have not defined a Null type.

- The Mult constructor does not make sense. It looks like they intended to use it to represent a directory containing multiple subdirectories, but they have not provided any arguments for the subdirectories.

- The Subdir constructor does not make sense. It looks like they intended to use it to represent a subdirectory, but they have not provided any arguments for the subdirectory.

Here is a better defition.
We can start by representing a file as a data type with two fields: a string for the name and an integer for the size. 
-}
data File = File String Int

-- And we can represent a directory as a data type with three fields: 
-- a string for the name, a list of subdirectories, and a list of files.
data Dir = Dir String [Dir] [File]


-- Here is an example of how we can use these data structures:
docsDir = Dir "documents" [] []

todoFile = File "todo.txt" 10

home = Dir "home" [docsDir] [todoFile]

-- This represents a directory called "home".
-- Home contains a subdirectory called "documents" and a file called "todo.txt" with size 10.


data STree a = SLeaf a | Empty | SNode (STree a) a (STree a) deriving Show

insert :: Ord a => STree a -> a -> STree a

-- Checks if the tree is empty - then returns a single-node tree containing the value x.
insert Empty x = SLeaf x

{-
Compare x with the value at the root node. 
If x is smaller, insert x into left subtree. 
If x is larger, insert x into right subtree. 
If x is equal to the value at the root node, returns the tree unchanged.
-}
insert (SLeaf y) x
    | x < y     = SNode (SLeaf x) y Empty
    | otherwise = SNode Empty y (SLeaf x)
insert (SNode l m r) x
    | x < m     = SNode (insert l x) m r
    | x > m     = SNode l m (insert r x)
    | otherwise = SNode l m r


-- We say that a binary tree is balanced if the number of leaves in every left and right subtree differ by at most one with leaves themselves being trivially balanced. 
-- Let us define a function that will tell us if a binary tree is balanced or not.

-- First, a function to count the number of leaves in a tree recursively:
numLeaves :: STree a -> Int
numLeaves (SLeaf _) = 1
numLeaves Empty = 0
numLeaves (SNode l _ r) = numLeaves l + numLeaves r


-- This function uses numLeaves and recursive calls to check the balance of the left and right subtrees:


balanced :: STree a -> Bool
balanced Empty = True
balanced (SLeaf _) = True
balanced (SNode l m r) = diff <= 1 && balanced l && balanced r
    where diff = abs (numLeaves l - numLeaves r)

{-
Here is an example of a balanced tree:
    5
   / \
  3   7
 / \ / \
1  4 6  8

As a Haskell data type:
-}
balancedTree = SNode (SNode (SLeaf 1) 2 (SLeaf 3)) 4 (SNode (SLeaf 5) 6 (SLeaf 7))


{-
Here is an example of an unbalanced tree:
  4
 / \
2   3
 \
  1
 
As a Haskell data type:
-}
unbalancedTree = SNode (SNode (SLeaf 1) 2 (SLeaf 3)) 4 Empty


-- Extra exercises --

{-
Here is an example instance declaration defining how the `Eq` typeclass should be
implemented for the `Maybe a` type, where `a` is an instance of the `Eq` typeclass.
This allows values of the `Maybe a` type to be compared for equality using the `==`
operator, as long as the type `a` is an instance of `Eq`.

instance Eq a => Eq (Maybe a) where 
    Nothing == Nothing = True
    Just x == Just y = x == y
    _ == _ = False

Of course, this is already something that is declared in the Haskell Prelude, and is
simply an illustration of how to instande declarations work.
-}

data Expression = Value Int | Addition Expression Expression

foldexp :: (Int -> a) -> (a -> a -> a) -> Expression -> a
foldexp f g (Value n) = f n
foldexp f g (Addition e1 e2) = g (foldexp f g e1) (foldexp f g e2)

evaluate :: Expression -> Int
evaluate = foldexp (^2) (+)

-- Here is a sample expression you can use to call eval, which raises each value to the power of 2 and adds them:
sampleExpr :: Expression
sampleExpr = Addition (Value 3) (Addition (Value 4) (Value 5))

-- build :: Ord a => [a] -> STree a
build :: Ord a => [a] -> STree a
build [] = Empty
build (x:xs) = insert (build xs) x