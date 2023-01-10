data UTree a = Node a [UTree a]

instance Functor UTree where
    fmap f (Node x xs) = Node (f x) (fmap (fmap f) xs)


{-
This is a really weird exercise because there's no way it can compile due to duplicate definitions.

instance Functor ((->)r) where
    fmap f g = \x -> f (g x)
-}


{-
This definition of (<*>) works by mapping the fmap function over the list of functions fs, and then concatenating the resulting lists.

(<*>) fs xs = concat (fmap (\f -> fmap f xs) fs)

I have commented it out though since the operator is already defined in the Haskell prelude, and I don't want errors from duplicate definitions in the bonus exercises.

The fmap (\f -> fmap f xs) fs part applies the fmap function to each element in the list of functions fs, resulting in a list of lists.
Each inner list is generated by applying the corresponding function from fs to all the elements in the list xs using fmap.

Usage in ghci:
let fs = [(+1), (*2)]
let xs = [1, 2, 3]

fs <*> xs
> [2,3,4,2,4,6]

This applies each function in the list fs to all the values in the list xs, resulting in a list with all the possible combinations of function application.
-}


data Expr a = Var a | Val Int | Add (Expr a) (Expr a)

instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap f (Add a b) = Add (fmap f a) (fmap f b)
    fmap _ (Val x) = Val x


instance Applicative Expr where
    pure x = Var x
    Var f <*> Var x = Var (f x)
    Add f g <*> Add x y = Add (f <*> x) (f <*> y)
    _ <*> _ = error "Invalid application."