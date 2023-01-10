tuple :: Monad m => m a -> m b -> m (a, b)
tuple a b =
    a >>= \x ->
    b >>= \y ->
    return (x, y)


-- tuple' is equivalent to tuple, but is written using do-notation instead of the bind operator
tuple' :: Monad m => m a -> m b -> m (a, b)
tuple' a b = do
    x <- a
    y <- b
    return (x, y)


g f z s = do
    y <- z
    s y
    return f y

-- g' is equivalent to g, but is written using the bind operator instead of do-notation
g' f z s =
    z >>= \y ->
    s y >>= \_ ->
    return f y


fourfirst xs = do
    x <- xs
    return (4, x)

{-
This fourfirst function does not work as intended because it binds x to xs.
We have to remember that, for the list monad, the bind operator is defined as follows:

xs >>= f = concat (map f xs)

This is also known as the flatMap or chain function.
The bind operator applies f to each element in the list using map,
and then concatenates the resulting lists using concat.

So here's what's actually happening, assuming we define xs = [1, 2, 3]:
-}

fourfirst' xs = xs
    >>= (\x -> return (4, x))

{-
The return function is used to wrap a value in a monad.
In this case, the return function is used to wrap the pair (4, x) in a list monad, resulting in a list of pairs.

This is because it's actually applying the function `return (4, x)`, which wraps the pair (4, x) in a list monad.

So what happens is actually this:
concat (map (\x -> return (4, x)) [1,2,3])
-}

newtype State = SE [Int] deriving Show
newtype ST a = ST (State -> (a, State))

app :: ST a -> State -> (a, State)
app (ST st) x = st x

get :: ST Int
get = ST (\(SE xs) -> case xs of
    [] -> (0, SE [])
    (x:xs) -> (x, SE (x:xs)))

put :: Int -> ST Int
put y = ST (\(SE xs) -> (y, SE (y : xs)))

remove :: ST Int
remove = ST (\(SE xs) -> case xs of
    [] -> error "Cannot remove elements from an empty stack."
    (x:xs) -> (x, SE xs))

{-
Example use of get, put and remove:
> app get (SE [1, 2, 3])
(1, SE [1,2,3])

> app (put 4) (SE [1, 2, 3])
(4, SE [1, 2, 3, 4])

> app remove (SE [1, 2, 3])
(1, SE [1, 2])
-}

-- Defining the ST monad
-- We must first make ST a functor

instance Functor ST where
   fmap f st = ST (\s -> let (x,s') = app st s in (f x,s'))

-- Then we must make ST an applicative functor

instance Applicative ST where
  pure x = ST (\s -> (x,s))
  stf <*> stx = ST (\s ->
                  let (f,s')  = app stf s
                      (x,s'') = app stx s' in (f x,s''))

-- Finally, we can make ST a monad; we only need to define >>= as return is simply the -- pure function

instance Monad ST where
  st >>= f = ST (\s ->
                let (x,s') = app st s in app (f x) s')


push :: Int -> ST Int
push x = do put x

pop = do remove

add = do
    x <- pop
    y <- pop
    push (x + y)

mult = do
    x <- pop
    y <- pop
    push (x * y)

prog = do
    push 2
    push 3
    add
    push 5
    mult
    pop

result = app prog (SE [0])


foldM :: Monad m => (t1 -> t2 -> m t2) -> [t1] -> t2 -> m t2
foldM _ [] acc = return acc
foldM f (x:xs) acc = do
    newAcc <- f x acc
    foldM f xs newAcc

dingo x = do
    putStrLn (show x)
    return x

dingoResult = foldM (\x y -> (dingo (x+y))) [1,2,3,4] 0