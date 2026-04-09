module Text_filter where

import Data.Char (toUpper, toLower)

menu :: IO ()
menu = do
    putStrLn "\nChoose an operation: "
    putStrLn "1  -  Uppercase"
    putStrLn "2  -  Lowercase"
    putStrLn "3  -  How many words does this text contain"
    putStrLn "4  -  How many times a given word appears in this text"

upAll :: String -> IO ()
upAll txt = putStrLn (map toUpper txt)

lowAll :: String -> IO ()
lowAll txt = putStrLn (map toLower txt)

countWords :: String -> IO ()
countWords text = putStrLn ("Number of words: " ++ show (length (words text)))

countOccurrences :: String -> IO ()
countOccurrences text = do
    putStrLn "Give the word to search for:"
    word <- getLine
    let count = length (filter (== word) (words text))
    putStrLn ("The word '" ++ word ++ "' appears " ++ show count ++ " times")

main :: IO ()
main = do
    putStrLn "Give me a text (Ctrl+D to finish):"
    txt <- getContents
    menu
    choice <- getLine
    case choice of
        "1" -> upAll txt
        "2" -> lowAll txt
        "3" -> countWords txt
        "4" -> countOccurrences txt
        _   -> putStrLn "Invalid operation"