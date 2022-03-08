{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Exam3 where

import Data.List hiding (nub)

data Bit = One | Zero deriving (Show, Eq)
type Nat = [Bit]

two :: Nat
two = [One,Zero]

eleven :: Nat
eleven = [One,Zero,One,One]

natToInt :: Nat -> Integer
natToInt x = sum $ aux (reverse x) 0 where
    aux [] e = []
    aux (y : ys) e = if(y == One) then (1 * 2^e) : aux ys (e + 1) else aux ys (e+1)

intToNat :: Integer -> Nat
intToNat x
    | x < 0 = error "Negative Number"
    | x >= 0 = reverse (aux x) where
        aux 0 = []
        aux x = (if(mod x 2 == 0) then Zero  else One  ) : aux (div x 2)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder One One Zero = (Zero, One)
fullAdder Zero Zero One = (One, Zero)
fullAdder One Zero One = (Zero, One)
fullAdder Zero One One = (Zero, One)
fullAdder One Zero Zero = (One, Zero)
fullAdder Zero One Zero = (One, Zero)
fullAdder Zero Zero Zero = (Zero, Zero)


addNat :: Nat -> Nat -> Nat
addNat x y = reverse $ aux (reverse x) (reverse y) Zero where
    aux [] [] z = if(z == Zero) then [] else [z]
    aux (a:as) [] z = let (s,c) = fullAdder a (Zero) z in s : aux as [] c
    aux [] (b:bs) z = let (s,c) = fullAdder (Zero) b z in s : aux [] bs c
    aux (a:as) (b:bs) z = let (s,c) = fullAdder a b z in s : aux as bs c


type InventoryList = [(String,Double,Integer)]

bookStore :: InventoryList
bookStore = [("Pilates for beginners", 29.90, 20),("Harry Potter 2021", 29.90, 0),("Sharks in the Inn", 8.90, 15),("Dolphins and wales", 15.99, 50)]

boundProducts :: InventoryList -> Double -> Double -> [String]
boundProducts x y z = [a | (a,b,c) <- x, b >= y, b <= z]

totalValue :: InventoryList -> Double
totalValue x = sum [b * (fromIntegral c)| (a,b,c) <- x]

expensiveProducts :: InventoryList -> [String]
expensiveProducts l@((a,b,c) : xs) = let max = aux l b  in [m | (m,n,o)<- (filter (\(a,b,c) -> b == max) l)] where
    aux [] acc = acc
    aux ((a,b,c) : xs) acc = if(acc < b) then aux xs b else aux xs acc


isNumber "" = False
isNumber l@(x : xs)
    | x == '-' =  null ([x | x <- xs, x < '0' || x > '9'])
    | otherwise =  null ([x | x <- l, x <  '0' ||  x  > '9'])

g::a -> a
g x = x

constant :: Bool 
constant = g(1968 ::Integer) == 8545 && g "input" == "input"

type First_Name = String
type Last_Name = String
type Country = String 
data PersonT = Person First_Name Last_Name Country 
newborns_2019 :: [PersonT]
newborns_2019 = [Person "Rainer" "Unsinn" "Germany",Person "Rainer" "Test" "USA", Person "Erik" "Friday" "USA", Person "Erik" "Test" "USA"]

all_names :: Country -> [First_Name]
all_names country = [ f | (Person f l c) <- newborns_2019, c == country]

check acc [] = [acc]
check acc xs
    | acc `elem` xs = xs
    | otherwise = acc : xs

nub :: Eq a => [a] -> [a] 
nub xs = foldr check [] xs

preferred_names :: Country -> [First_Name]
preferred_names country = sort $ nub $ all_names country

fact n
    | n == 0 = 1
    | n > 0 = n * fact (n - 1)