{-
    Given a function concat :: [[a]] -> [a], briefly explain the type of the following six Haskell expressions
    or give a reason why these expressions result in a type error.

    concat :: [[a]] -> [a]

    e1 = concat [1 :: Int, 2, 3] --false because the give List is not a nested List 
    e2 = concat ["one", "two", "three"] --true, Strings ar List of Char
    e3 = concat [[1 :: Int, 2], [], [3]] --
    e4 = concat [["one", "two"], [], ["three"]] -- true, 
    e5 = concat e3 -- false, because of return
    e6 = concat e4 -- true
-}


------------------------------------------- Exercise 1.2
getTail :: [a] -> [a]
getTail [] = []
getTail (x:xs) = xs

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
-- suffixes l@(x:xs) = l : suffixes xs
-- suffixes x = x : suffixes(tail x)
suffixes x = x : suffixes(getTail x) -- x : (speichern von x)


------------------------------------------- Exercise 1.3
withoutLast:: [a] -> [a]
withoutLast [] = []
withoutLast [x] = []
withoutLast (x:xs) = x : (withoutLast xs)

prefixes :: [a] -> [[a]]      
prefixes [] = [[]]
-- prefixes x = x : prefixes(init x)-- and the definition
prefixes x = x : prefixes(withoutLast x)-- and the definition


------------------------------------------- Exercise 1.4

menu :: Char -> [a] -> Either String [[a]]
menu 's' xs = Right(suffixes xs) 
menu 'p' xs = Right(prefixes xs)
menu o _ = Left("(" ++ show o ++ ") is not allowed. Use (p)refix or (s)uffix ")



------------------------------------------- Exercise 2
-- Exercise 2.1
data Expr a = 
  Plus (Expr a) (Expr a)
  | Times (Expr a) (Expr a)
  | Number a
  deriving Show 


expr1 = Times (Plus (Number (5.2 :: Double)) (Number 4)) (Number 2)
expr2 = Plus (Number (2 :: Int)) (Times (Number 3) (Number 4))
expr3 = Times (Number "hello") (Number "world") -- is correct
-- Exercise 2.2
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys


numbers :: Expr a -> [a]    
numbers (Number x) = [x]
numbers (Plus x y) = append (numbers x) (numbers y)
-- numbers (Plus x y) = (numbers x) ++ (numbers y)
numbers (Times x y) = append (numbers x) (numbers y)
-- numbers (Times x y) = (numbers x) ++ (numbers y)

-- Exercise 2.3
eval :: (Num a, Show a) => Expr a -> a       
eval (Number x) = x
eval (Plus x y) = eval x + eval y
eval (Times x y) = eval x * eval y 

-- Exercise 2.4

exprToString :: Show a => Expr a -> String     
exprToString (Number x) = show x 
exprToString (Plus x y) = exprToString x ++ " + " ++ exprToString y
exprToString (Times x@(Plus _ _) y) = "(" ++ exprToString x ++ ") * " ++ exprToString y
exprToString (Times y x@(Plus _ _) ) = exprToString y ++ " * (" ++ exprToString x ++ ")" 
exprToString (Times x y) = exprToString x ++ " * " ++ exprToString y

data Type a = Empty | Node a Int (Type a) deriving Eq

d = \x -> Node x x Empty

-- Tests: Un-comment the desired test (and :reload) after you provided a corresponding solution.
testSuffixes = "Expected [[1,2,3],[2,3],[3],[]]; suffixes [1,2,3] returned " ++ show (suffixes [1,2,3] :: [[Int]])
-- testPrefixes = "Expected [[1,2,3],[1,2],[1],[]]; prefixes [1,2,3] returned " ++ show (prefixes [1,2,3] :: [[Int]])
-- testMenuP = "Expected Right [[1,2],[1],[]]; menu 'p' [1,2] returned " ++ show (menu 'p' [1,2] :: Either String [[Int]])
-- testMenuS = "Expected Right [[1,2],[2],[]]; menu 's' [1,2] returned " ++ show (menu 's' [1,2] :: Either String [[Int]])
-- testMenuC = "Expected Left \"(d) is not supported, use (p)refix or (s)uffix\"; menu 'd' [1,2] returned " ++ show (menu 'd' [1,2] :: Either String [[Int]])
-- testMenu = putStr (testMenuP ++ "\n" ++ testMenuS ++ "\n" ++ testMenuC ++ "\n")

-- testEval1 = "Expected 18.4; eval expr1 returned " ++ show (eval expr1 :: Double)
-- testEval2 = "Expected 14; eval expr2 returned " ++ show (eval expr2 :: Int)
-- testEval = putStr (testEval1 ++ "\n" ++ testEval2 ++ "\n")

-- testExprToString1 = "Expected \"(5.2 + 4.0) * 2.0\"; exprToString expr1 returned " ++ show (exprToString expr1 :: String)
-- testExprToString2 = "Expected \"2 + 3 * 4\"; exprToString expr2 returned " ++ show (exprToString expr2 :: String)
-- testExprToString3 = "Expected \"\\\"hello\\\" * \\\"world\\\"\"; exprToString expr3 returned " ++ show (exprToString expr3 :: String)
-- testExprToString = putStr (testExprToString1 ++ "\n" ++ testExprToString2 ++ "\n" ++ testExprToString3 ++ "\n")
