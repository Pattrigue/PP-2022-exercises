-- Discussion exercises -- 

hello :: IO()
hello = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")

{-
In Haskell, the sequence_ function is a way to sequence a list of actions, discarding the results of each action and returning only the unit value ().
This is useful when you want to perform several actions in a row, but are only interested in the side effects of those actions and don't care about the return values.
Note the difference between sequence and sequence_:
sequence takes a list of monadic values and returns a monadic value with a list.
sequence_ does the same but discards the return result and just returns the unit value ().
-}

-- Prints "riprap"
sequence1 = sequence_[putStr "rip", putStr "rap", return ()]

{-
Errors because getChar returns IO Char, and not the unit value () like putStr
sequence2 = sequence_[putStr "rip", putStr "rap", return getChar]

In other words, sequence_ expects the return value of each monadic element in the list to be the same.
Can be fixed by simply discarding the return value of getChar:
-}
sequence2 = sequence_[putStr "rip", putStr "rap", getChar >>= \_ -> return ()]


-- Class exercises --
{-
main = do
    w <- getLine 
    loop ((read) w :: Int)
    where
        loop 1 = putStr (show 1)
        loop x = do
            putStr (show x)
            if even x
                then loop (x `div` 2)
                else loop (3*x + 1)

This program reads a line from user input using getLine, and then calls loop with that input converted to an integer.
loop simply prints "1" if the value is 1, otherwise it goes into a recursive loop.
Here, it prints the value, then checks if it's even.
If it's even, it recursively calls itself with the value divided by 2.
If it's not even, it recursively calls itself with the value multiplied by 3 and added by 1.
It continues doing this until the value is 1, in which case it reaches the base case and terminates.
This program is an implementation of the Collatz function.
-}


letters :: IO ()
letters = do
    input <- getLine
    output input
        where
            output [] = return ()
            output w = do
                putChar (head w)
                putChar '\n'
                output (tail w)


-- Here is an equivalent function using sequence_:
letters' :: IO ()
letters' = do
    input <- getLine
    output input
        where
            output [] = return ()
            output w = sequence_ [putChar (head w), putChar '\n', output (tail w)]


hugorm :: IO ()
hugorm = do
    putStr "How many numbers would you like to add? "

    input <- getLine
    values <- getValues (read input :: Int) []

    putStrLn ("The sum is " ++ show (sum values))

    where
        getValues 0 xs = return xs
        getValues n xs = do
            input <- getLine
            getValues (n - 1) ((read input :: Int) : xs)


-- Extra problems --
sumInts :: Integer -> IO Integer
sumInts x = do
    input <- getLine

    let n = read input :: Integer

    if n == 0 then do
        putStr "The sum is: "
        return x
    else
        sumInts (x + n)


-- Here is a generalized version of sumInts by implementing a whileIO function:
whileIO :: IO a -> (a -> Bool) -> (a -> a -> a) -> a -> IO a
whileIO getIO condF foldF x = do
    n <- getIO

    if condF n then
        return x
    else
        whileIO getIO condF foldF (foldF x n)


sumInts' :: IO ()
sumInts' = do
    sum <- whileIO getIO condF foldF 0
    print sum
    where
        getIO = do
            input <- getLine
            return (read input :: Integer)
        condF n = n == 0
        foldF x n = x + n
