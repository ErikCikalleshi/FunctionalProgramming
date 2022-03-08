module Main (main) where
import System.IO

main = do
    x <- getLine 

    if(x == "") then return () else do
        let y = splitAtColon x
        let num = aux y 0
        putStrLn (show num ++ " number(s)")
        main

aux [] n = n
aux (x:xs) n = if(isNumeric x) then aux xs (n+1) else aux xs n 


isNumeric :: String -> Bool
isNumeric "" = False
isNumeric l@(x : xs)
    | x == '-' =  null ([x | x <- xs, x < '0' || x > '9'])
    | otherwise =  null ([x | x <- l, x <  '0' ||  x  > '9'])



splitAtColon :: String -> [String]
splitAtColon [] = []
splitAtColon xs = takeWhile (/= ':') xs : splitAtColon (aux (dropWhile (/= ':') xs)) where
    aux [] = []
    aux (x:xs) = xs