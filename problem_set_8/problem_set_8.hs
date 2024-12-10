-- Week 8. Problem set
-- Evgeny Bobkunov SD-03 e.bobkunov@innopolis.university

-- 1. What are the types of secret and p in the following program? Justify your answer.

secret g = do
  s <- getLine
  x <- g s
  case p x of
    Nothing -> secret g
    Just y -> return (p (length y))

-- Type of getLine:
-- getLine :: IO String, This means s will have the type String.

-- Type of g: 
-- g is a function that takes s (a String) as input, so the type of g is String -> IO a for some type a

-- Type of x:
-- Since x is the result of g s, x has type a. So, x :: a.

-- Type of p:
-- p x is matched in a case expression with Nothing and Just y.
-- This means p must be a function that takes x (of type a) and returns a Maybe b for some type b.
-- So, the type of p is a -> Maybe b.

-- Type of y:
-- In the Just y case, y has type b.

-- Type of length y:
-- length :: [a] -> Int works on lists.
-- For length y to make sense, y must be a list, i.e., y :: [c] for some type c. Hence, b = [c].

-- Type of p (length y) in return (p (length y)):
-- length y :: Int, and p is applied to x (of type a), so p must also handle Int values in the second application.
-- Therefore, p has the type Int -> Maybe d (it can take an Int as input),
-- but from earlier analysis, it must also have the type a -> Maybe [c].

-- g :: String -> IO a
-- p :: a -> Maybe [c] and Int -> Maybe d


-- 2. Implement a program echoByWord :: IO () that goes through an infinite loop of reading user input and printing it back in modified form

import Data.Char (toUpper, isAlpha)

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : filter isAlpha xs

processInput :: String -> String
processInput input = unwords (map (\w -> capitalizeWord w ++ ".") (words filteredInput))
  where
    filteredInput = filter (\c -> isAlpha c || c == ' ') input

echoByWord :: IO ()
echoByWord = do
  line <- getLine
  putStrLn (processInput line)
  echoByWord

main :: IO ()
main = echoByWord 


-- 3. Implement the following functions over IO:



-- | (a) Run a given IO action forever

import Control.Monad (forever)

foreverIO :: IO a -> IO b
foreverIO action = forever action

main :: IO ()
main = foreverIO (putStrLn "Hello!")


-- | (b) Run a given IO action unless the condition is satisfied

import Control.Monad (when)

unlessIO :: Bool -> IO () -> IO ()
unlessIO condition action = when (not condition) action

main :: IO ()
main = do
    unlessIO False (putStrLn "Condition is NOT satisfied!")
    unlessIO True (putStrLn "This will NOT be printed.")

-- | (c) Run a given IO action if it is present, returning Maybe result

maybeIO :: Maybe (IO a) -> IO (Maybe a)
maybeIO Nothing = return Nothing
maybeIO (Just action) = do
    result <- action
    return (Just result)

main :: IO ()
main = do
    result1 <- maybeIO (Just (putStrLn "Hello, World!" >> return 42))
    print result1

    result2 <- maybeIO Nothing :: IO (Maybe Int)
    print result2



-- | (d) Run a sequence of IO (Maybe a) actions and collect the results

import Control.Monad (mapM)

sequenceMaybeIO :: [IO (Maybe a)] -> IO [a]
sequenceMaybeIO actions = do
    results <- mapM id actions
    return [a | Just a <- results]

main :: IO ()
main = do
    results <- sequenceMaybeIO 
        [ putStrLn "Action 1" >> return (Just 1 :: Maybe Int)
        , putStrLn "Action 2" >> return (Just 2 :: Maybe Int)
        , return Nothing
        , putStrLn "Action 3" >> return (Just 3 :: Maybe Int)
        ]
    print results

-- | (e) Run a program repeatedly until it returns Nothing

untilNothingIO :: (a -> IO (Maybe a)) -> a -> IO ()
untilNothingIO f initialValue = do
    result <- f initialValue
    case result of
        Nothing -> return ()
        Just nextValue -> untilNothingIO f nextValue

exampleAction :: Int -> IO (Maybe Int)
exampleAction x = do
    putStrLn $ "Current value: " ++ show x
    if x <= 0 
        then return Nothing
        else return (Just (x - 1))

main :: IO ()
main = untilNothingIO exampleAction 5

-- | (f) Traverse a list while maintaining a state

import Control.Monad (foldM)

forStateIO :: s -> [a] -> (s -> a -> IO (s, b)) -> IO (s, [b])
forStateIO initialState list stepFunction = do
    foldM updateState (initialState, []) list
  where
    updateState (state, results) item = do
        (newState, result) <- stepFunction state item
        return (newState, results ++ [result])

printConsLength :: Int -> String -> IO (Int, Int)
printConsLength totalLength s = do
    let n = length s
    let newTotalLength = totalLength + n
    putStrLn ("adding " ++ show s ++ " (total length = " ++ show newTotalLength ++ ")")
    return (newTotalLength, n)

main :: IO ()
main = do
    (finalState, results) <- forStateIO 0 ["hi", "world", "!"] printConsLength
    print (finalState, results)

-- 3. Implement a polymorphic higher-order function iforIO_ that runs a program for each element and its index in a given list (using given function).

import Control.Monad (forM_)

iforIO_ :: (Integral b) => [a] -> (b -> a -> IO ()) -> IO ()
iforIO_ xs action = forM_ (zip [0..] xs) $ \(i, x) -> action i x

example :: IO ()
example = do
    iforIO_ [1, 2] (\i n ->
        iforIO_ "ab" (\j c ->
            print ((i, j), replicate n c)))

main :: IO ()
main = example
