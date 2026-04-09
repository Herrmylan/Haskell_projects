module List_analyzer where

import Data.List (sort)

menu :: IO ()
menu = do
    putStrLn "\nChoose an operation: "
    putStrLn "1  -  Length of the given list"
    putStrLn "2  -  Sum of elements"
    putStrLn "3  -  Greater element"
    putStrLn "4  -  Lower element"
    putStrLn "5  -  Average of elements"
    putStrLn "6  -  Does the given list contain the given element"
    putStrLn "7  -  Nth element"
    putStrLn "8  -  Ascending order"
    putStrLn "9  -  Descending order"
    putStrLn "10 -  First n elements"
    putStrLn "11 -  Does the given list contain more of the given element"

len :: [a] -> IO ()
len list = putStrLn ("Length: " ++ show (length list))

summa :: (Num a, Show a) => [a] -> IO ()
summa list = putStrLn ("Sum of elements: " ++ show (sum list))

maxElem :: (Ord a, Show a) => [a] -> IO ()
maxElem list = putStrLn ("Greatest element in the list: " ++ show (maximum list))

minElem :: (Ord a, Show a) => [a] -> IO ()
minElem list = putStrLn ("Lowest element in the list: " ++ show (minimum list))

average :: (Fractional a, Show a) => [a] -> IO ()
average list = do
    let sum' = sum list
    let length' = fromIntegral (length list)
    putStrLn ("Average of the list: " ++ show (sum' / length'))

isElement :: (Eq a, Read a, Show a) => [a] -> IO ()
isElement list = do
    putStrLn "Give an element: "
    e <- readLn
    if (elem e list) then
        putStrLn "Yes"
    else
        putStrLn "No"

nThElement :: (Show a) => [a] -> IO ()
nThElement list = do
    putStrLn "Give a serial number: "
    s <- readLn :: IO Int
    if (s > 0 && s <= length list) then
        putStrLn ("The " ++ show s ++ ". element: " ++ show (list !! (s - 1)))
    else
        putStrLn "Index out of bounds"

ascending :: (Ord a, Show a) => [a] -> IO ()
ascending list = putStrLn (show (sort list))

descending :: (Ord a, Show a) => [a] -> IO ()
descending list = putStrLn (show (reverse (sort list)))

firstN :: (Show a) => [a] -> IO ()
firstN list = do
    putStrLn "Give a number: "
    n <- readLn :: IO Int
    putStrLn (show (take n list))

moreOf :: (Eq a, Read a, Show a) => [a] -> IO ()
moreOf list = do
    putStrLn "Give an element: "
    e <- readLn
    let moreElements = length (filter (== e) list)
    if (moreElements == 0) then
        putStrLn ("The element " ++ show e ++ " is not in the list")
    else if (moreElements == 1) then
        putStrLn ("The element " ++ show e ++ " appears once")
    else
        putStrLn ("The element " ++ show e ++ " appears " ++ show moreElements ++ " times")

main :: IO ()
main = do
    putStrLn "Give numbers separated by spaces:"
    input <- getLine
    let list = map read (words input) :: [Double]
    menu
    op <- getLine
    case op of
        "1"  -> len list
        "2"  -> summa list
        "3"  -> maxElem list
        "4"  -> minElem list
        "5"  -> average list
        "6"  -> isElement list
        "7"  -> nThElement list
        "8"  -> ascending list
        "9"  -> descending list
        "10" -> firstN list
        "11" -> moreOf list
        _    -> putStrLn "Invalid operation"