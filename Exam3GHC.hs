module Main (main) where

import Exam3
import System.IO

main = do
    putStrLn "Enter some numbers please: "
    helper where
        helper =  do
            x <- getLine 
            if(x == "0") then return () else do
                putStrLn ("Fact says: " ++ show(fact (read x :: Integer)))
                helper
       
    





-- main = do
--     putStrLn "Enter some numbers."
--     retry 0

-- retry :: Integer -> IO ()
-- retry y = do
--     x <- getLine 
--     if(isNumber x) then retry $ y + (read x :: Integer) else putStr $ "The sum of the number is"  ++ show y

