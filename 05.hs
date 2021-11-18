ite :: Bool -> a -> a -> a
ite True x y = x
ite False x y = y

-- Exercise 1.1
mergeLists :: [a] -> [a] -> [(a,a)]
mergeLists [] [] = []
mergeLists _ [] = []
mergeLists [] _ = []
mergeLists x y  = (head x, head y) : mergeLists (tail x) (tail y)
-----------------------------------------
mergeLists (x:xs) (y:ys)  = (x, y) : mergeLists (tail x) (tail y)
mergeLists _ _ = []

-- Exercise 1.2
calculateAge :: (Int, Int, Int) -> Int
calculateAge (x, y, z) = ite (x > 31 && y > 12) 0 (2021 - z - (ite (y <= 11 && x <= 10) 0 1))

-- Exercise 1.3
convertDatesToAges :: [(String, (Int, Int, Int))] ->  [(String, Int)]
convertDatesToAges [] = []
convertDatesToAges ((n, (x, y, z)) : xs) = (n, (calculateAge (x, y, z))) : convertDatesToAges xs

-- Exercise 1.4
getOtherPairValue :: (String, Int) -> Either String Int-> Maybe (Either String Int)
getOtherPairValue (n, age) (Left x) = ite (n == x) (Just (Right age)) Nothing
getOtherPairValue (n, age) (Right x) = ite (age == x) (Just (Left n)) Nothing


{-- Exercies 2.1
addPair (x, y) = x + y 
  Num a => (a, a) -> a
  takes a Tuple and adds the left side with the right side, therfore is the type Num a

addList [] = []
addList (x : xs) = addPair x : addList xs
    Num a => [a] -> [a]
--}
addPair :: Num a => (a, a) -> a
addPair (x, y) = x + y
addList :: Num a => [(a,a)] -> [a]
addList [] = []
addList (x : xs) = addPair x : addList xs

{--
  addList ((1,2):(2,1):[])

    addList ((1,2):((2,1):[])) = addPair (1,2) : addList ((2,1):[])
      addPair (1,2) = 1 + 2 # returns 3
    #recursive call
                          [3]
    addList ((2,1):[]) = addPair (1,2) : addList([])
      addPair (2,1) = 2 + 1 # returns 3
    #recursive call
    addList [] = []
    return List [(3,3):[]]
    
--}
-- Exercise 2.3
fstList :: [(a, b)] -> [a]
fstList [] = []
fstList ((x, y) : xs) = x : fstList xs 

-- Exercise 2.4
getLength :: [a] -> Int
getLength [] = 0
getLength (_:xs) = 1 + getLength xs

getSum :: Num a => [a] -> a
getSum [] = 0
getSum (x:xs) = x + getSum xs

getMax :: Ord a => [a] -> a -> a
getMax [] max = max
getMax (x:xs) max = ite (x > max) (getMax xs x) (getMax xs max)

lengthSumMax :: (Num a, Ord a) => [a] -> (Int, a, a)
lengthSumMax x = ((getLength x), (getSum x), (getMax x 0))

-- Tests
testMergeLists = do
  expected "[(1,'a'),(2,'b'),(3,'c')]"
 -- computed (mergeLists [1,2,3,4] ['a','b','c'] :: [(Int, Char)])
testCalculateAge = do
  expected "0"
  computed (calculateAge (10, 11, 2021))
  expected "20"
  computed (calculateAge (12, 12, 2000))
testConvertToAges = convertDatesToAges [("Erik", (13,12,2001)), ("Test", (13,12,2001))]
testFstList = do
  expected "[1,2,3]"
  computed (fstList [(1,'a'),(2,'b'),(3,'c')])
  expected "[\"hello\"]"
  computed (fstList [("hello","world")])
  expected "[]"
  computed (fstList [] :: [Integer])
testLengthSumMax = do
  expected "(0,0,0)"
  computed (lengthSumMax [])
  expected "(5,3,2)"
  computed (lengthSumMax [0,1,0,2,0])

expected e = putStrLn ("expected: " ++ e)
computed c = putStrLn ("computed: " ++ show c)
