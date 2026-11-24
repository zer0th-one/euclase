module Main where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStr "[Polish Notation type checker]> " -- example: "string int list map"
    hFlush stdout
    input <- getLine
    if input == "quit" then
        putStrLn "Quitting..."
    else do
        putStrLn "Computing..."
        putStrLn $ validType input
        main

data Result = Valid | Invalid String

arities :: [(String, Int)]
arities = [
        ("int"   , 0),
        ("bool"  , 0),
        ("string", 0),
        ("list"  , 1),
        ("map"   , 2)
    ]

arity :: String -> Maybe Int
arity kw = lookup kw arities

checkType :: [String] -> Int -> Result
checkType [] c
    | c == 0    = Invalid "no type provided"
    | c == 1    = Valid
    | otherwise = Invalid "mismatched types" -- o algo
checkType (w:ws) c =
    case arity w of
        Nothing -> Invalid $ "invalid type: " ++ w
        Just n  ->
            if n == 0 then
                checkType ws (c+1)
            else if c >= n then
                checkType ws (c-n+1)
            else Invalid $ "missing arguments for " ++ w

formatResult :: Result -> String
formatResult Valid            = "Type is valid"
formatResult (Invalid reason) = "Type is invalid (" ++ reason ++ ")"

validType :: String -> String
validType xs = formatResult $ checkType (words xs) 0
