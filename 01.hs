
milesToKilometers m = m * 1.609347

volume r = (4/3) * pi * r ** 3

average x y = (x + y)/2

{- Exercise 2 ->   Nr 4. 
    When executing "average (average x y) z" it actually calculates first the average in Brackets between two numbers and the results is beeing used
    as x for the average with z 
-}

averageVolume r1 r2 = average (volume r1)(volume r2)

twoListFun (x:xs) ys = twoListFun xs $ take 5 (x : ys)


fun :: Eq a => [Maybe a] -> Maybe a -> Maybe a
fun x y
  | y == Nothing = foldr g y x
  | otherwise = h x

g :: a -> b -> b
test x = foldr (\ x y -> x . y . y) [1,2,3,4] x
g = undefined
h :: [a] -> a
h = undefined


-- the following tests can be used by you, once you have implemented your functions,
-- for testing, just invoke test1, test2, ... in ghci

test1 = "test1: expected ~ 107.826048, computed: " ++ show (milesToKilometers 67 :: Double)
test2 = "test2: expected ~ 179.59438, computed: " ++ show (volume 3.5 :: Double)
test3 = "test3: expected ~ 2.070796, computed: " ++ show (average pi 1 :: Double)
test4 = "test4: expected ~ 150.796448, computed: " ++ show (averageVolume 2 4 :: Double)

