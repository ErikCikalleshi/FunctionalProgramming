-- Definition of List from lecture
data List = Empty | Cons Integer List deriving Show
-- example list for testing
list = Cons 1 (Cons 7 (Cons 9 (Cons 3 Empty)))
-- example assignment for testing
myAssn = Assign "x" 1 (Assign "x" 2 (Assign "y" 3 EmptyA))


-- Exercise 2

-- disj
disj :: Bool -> Bool -> Bool
disj x y = x || y 

-- sumList
sumList :: List -> Integer
sumList Empty = 0;
sumList (Cons x xs) = x + sumList xs

-- double2nd
double2nd :: List -> List
-- double2nd Empty = Empty
-- double2nd (Cons _ Empty) = list
double2nd (Cons x (Cons y xs)) = (Cons x (Cons(y*2) (double2nd(xs))))
double2nd xs = xs;

-- Exercise 3

data Expr = Number Integer
  | Var String
  | Plus Expr Expr
  | Negate Expr
  deriving Show
  
data Assignment = EmptyA | Assign String Integer Assignment
  deriving Show

ite :: Bool -> Integer -> Integer -> Integer
ite True x y = x
ite False x y = y

lookupA :: Assignment -> String -> Integer
lookupA EmptyA z = 0
--last Element: check and if false return 0
lookupA (Assign x y EmptyA) z = (ite (x == z) y 0)
lookupA (Assign x y xs) z  = (ite (x == z) y (lookupA xs z))

eval :: Assignment -> Expr -> Integer
eval _ (Number x) = x
eval assn (Negate e) = - eval assn e
eval assn (Plus e1 e2) =  eval assn e1 + eval assn e2
eval assn (Var s) = lookupA assn s 
  

-- Exercise 3.4

{- You may (and will have to) extend this datatype in order to incorporate the "let" construct. -}
data Expr' = Number' Integer
  | Var' String
  | Plus' Expr' Expr'
  | Negate' Expr'
  | Let' String Expr' Expr'
  deriving Show

  
eval' :: Assignment -> Expr' -> Integer
eval' assn (Number' x) = x
eval' assn (Negate' e) = - eval' assn e
eval' assn (Plus' e1 e2) =  eval' assn e1 + eval' assn e2
eval' assn (Var' s) = lookupA assn s 
-- eval' assn (Let' x y) = eval' (Assign "x" (eval' assn x)) y 
eval' assn (Let' n x y) = eval' (Assign n (eval' assn x) assn) y

eval2 = eval' myAssn (Let' "x" (Plus' (Number' 3) (Number' 3)) (Plus' (Var' "x")(Var' "x")))




-- the following tests can be used by you, once you have implemented your functions,
-- for testing, just invoke testX in ghci
testSum = putStrLn ("expected: 20\ncomputed: " ++ show (sumList list))
testDouble = putStrLn ("expected: Cons 1 (Cons 14 (Cons 9 (Cons 6 Empty)))\ncomputed: " ++ show (double2nd list))
testLookupA = putStrLn ("expected: 1 3 0\ncomputed: " ++ show (lookupA myAssn "x") ++ " " ++ show (lookupA myAssn "y") ++ " " ++ show (lookupA myAssn "z"))
testEval = putStrLn ("expected: 42\ncomputed: " ++ show (eval myAssn (Plus (Negate (Var "y")) (Number 45))))


-- Excersice 1
data Subject = CS | Math | Physics | Biology  
data Programme = 
    Bachelor Subject
  | Master Subject
  | Teaching Subject Subject --Teachers need two subjects
data Student = Student 
    String --name
    Integer -- matriculation number
    Bool --active inscription
    Programme


f1 (Student name n _ (Teaching Math _)) = True
f1 _  = False

f2 (Student name n False  p@(Master _)) = True
f2 _ = False

{- 
data Subject = CS | Math | Physics | Biology   deriving Show
data Programme = 
    Bachelor Subject
    | Master Subject
    | Teaching Subject Subject --Teachers need two subjects
  deriving Show
data Student = Student 
    String --name
    Integer -- matriculation number
    Bool --active inscription
    Programme
  deriving Show

Student "Jane Doe" 243781 True (Teaching Math Physics) i --> name / "Jane Doe", n / 243781 _, Subject / Teaching Math _
Student "Max Meyer" 221341 False (Teaching CS Math) None 
Student "Mary Smith" 234145 False (Master CS) ii --> name / "Jane Doe", n / 243781 _, Subject / Teaching Math

i. Student name n _ (Teaching Math _) 
ii. Student name n False  p@(Master _) 
-}





