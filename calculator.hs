module Calculator where

import Data.List (foldl')

menu :: IO ()
menu = do
    putStrLn "Choose an operation: "
    putStrLn "1  -  Add"
    putStrLn "2  -  Difference"
    putStrLn "3  -  Product"
    putStrLn "4  -  Division"
    putStrLn "5  -  Power"
    putStrLn "6  -  Factorial"
    putStrLn "7  -  N-th root"
    putStrLn "8  -  Degree to radian"
    putStrLn "9  -  Radian to degree"
    putStrLn "10 -  Trigonometric functions"

add :: IO ()
add = do
    putStrLn "Give two numbers: "
    a <- readLn
    b <- readLn
    let result = show (a + b)
    putStrLn (show a ++ " + " ++ show b ++ " = " ++ result)

dif :: IO ()
dif = do
    putStrLn "Give two numbers: "
    a <- readLn
    b <- readLn
    let result = show (a - b)
    putStrLn (show a ++ " - " ++ show b ++ " = " ++ result)

prod :: IO ()
prod = do
    putStrLn "Give two numbers: "
    a <- readLn
    b <- readLn
    let result = show (a * b)
    putStrLn (show a ++ " * " ++ show b ++ " = " ++ result)

divi :: IO ()
divi = do
    putStrLn "Give two numbers: "
    a <- readLn :: IO Double
    b <- readLn :: IO Double
    if b == 0 then
        putStrLn "No division by 0"
    else do
        let result = show (a / b)
        putStrLn (show a ++ " / " ++ show b ++ " = " ++ result)

pow :: IO ()
pow = do
    putStrLn "Give two numbers: "
    a <- readLn :: IO Double
    b <- readLn :: IO Double
    if (a == 0 && b == 0) then
        putStrLn "Power of 0 and 0 doesn't exist"
    else do
        let result = show (a ** b)
        putStrLn (show a ++ " ^ " ++ show b ++ " = " ++ result)

fact :: IO ()
fact = do
    putStrLn "Give a number: "
    n <- readLn :: IO Integer
    let result = foldl' (*) 1 [1..n] :: Integer
    putStrLn (show n ++ "! = " ++ show result)

root :: IO ()
root = do
    putStrLn "Give two numbers: "
    n <- readLn :: IO Double
    a <- readLn :: IO Double
    if n == 0 then
        putStrLn "0th root doesn't exist"
    else do
        let result = show (a ** (1 / n))
        putStrLn (show a ++ " ^ (1 / " ++ show n ++ ") = " ++ result)

degToRad :: IO ()
degToRad = do
    putStrLn "Give an angle in degrees"
    x <- readLn :: IO Double
    let result = show (x * pi / 180)
    putStrLn (show x ++ "° = " ++ result ++ " rad")

radToDeg :: IO ()
radToDeg = do
    putStrLn "Give an angle in radian"
    x <- readLn :: IO Double
    let result = show (x * 180 / pi)
    putStrLn (show x ++ "rad = " ++ result ++ "°")

trig :: IO ()
trig = do
    putStrLn "Give an angle in degrees"
    x <- readLn :: IO Double
    let rad = x * pi / 180 :: Double
    putStrLn ("sin(" ++ show x ++ "°) = " ++ show (sin rad))
    putStrLn ("cos(" ++ show x ++ "°) = " ++ show (cos rad))
    putStrLn ("tan(" ++ show x ++ "°) = " ++ show (tan rad))
    putStrLn ("cot(" ++ show x ++ "°) = " ++ show (1 / tan rad))

main :: IO ()
main = do
    menu
    op <- getLine
    case op of
        "1"  -> add
        "2"  -> dif
        "3"  -> prod
        "4"  -> divi
        "5"  -> pow
        "6"  -> fact
        "7"  -> root
        "8"  -> degToRad
        "9"  -> radToDeg
        "10" -> trig
        _    -> putStrLn "Invalid operation"



