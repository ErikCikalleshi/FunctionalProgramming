import System.IO


-- Exercise 1

dividesRange :: Integer -> Integer -> Integer -> Bool
dividesRange n l u = if(l <= u) then 
                        if((mod n l) == 0) then True else dividesRange n (l+1) u   
                    else 
                        False
  
prime :: Integer -> Bool
prime 0 = False
prime 1 = False
--prime x = if((dividesRange x 2 (x-1)) == False) then True else False
prime n = let x = sqrtInt(n) in if((dividesRange n 2 x) == False) then True else False

generatePrime :: Integer -> Integer
generatePrime x = intPrime(10^(x-1)) where -- start with x digits
     intPrime n
       | prime n = n 
       | otherwise = intPrime(n + 1)


{-excersise 1.4
  with d = 8 in a few seconds
  with d = 9 in 01:10
-}    
sqrtInt :: Integer -> Integer
sqrtInt n = aux 0 n
  where
    aux x n
      | x*x < n = aux (x+1) n
      | otherwise = x


-- Exercise 2
heron :: Double -> [Double]
heron n
  | n == 0 || n == 1 = [sqrt(n)]
  | otherwise = aux (hheron n) n
      where
        aux a x
          | a == (getNext a x) = [a]
          | otherwise = a : aux ((a + (x / a))/2) x

getNext a x = ((a + (x / a))/2)

hheron :: Double -> Double
hheron x = aux 0 x
  where
    aux n s 
      | (n^2) < s = aux (n+1) s
      | s == 1 || s == 0 = sqrt(s)
      | otherwise = sqrt((n-1)^2)

-- Exercise 3
--struggel already with 100
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)
        
--struggle with 1.000.000
fib' :: Integer -> Integer
fib' 0 = 0
fib' 1 = 1
fib' 2 = 1
fib' n 
  | even n = (2 * fib'((div n 2) + 1) - fib'(div n 2)) * fib'(div n 2)
  | odd n = (fib'(div n 2))^2 + (fib'((div n 2) + 1))^2



fibFast :: Integer -> Integer
fibFast 0 = 0
fibFast 1 = 1
fibFast n = case fibFastAux n [(2,1),(1,1),(0,0)] of (x, y) -> x

fibFastAux :: Integer -> [(Integer, Integer)] -> (Integer, [(Integer, Integer)])
fibFastAux n list = case lookup n list of
                      Just x -> (x, list)
                      Nothing -> (fib' n, list)

-- fibFastAuxHelper :: Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
-- fibFastAuxHelper i x l = if (i <= x) then (i, (fib' i)) : fibFastAuxHelper (i + 1) x l else l ++ []



-- Tests
testDividesRange = do
  expected "True"
  computed (dividesRange 629 15 25)
  expected "False"
  computed (dividesRange 1009 30 500)
testPrime = do
  expected "[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]"
  computed (filter prime [0..100])
testGeneratePrime = do
  expected "[1009,100003,10000019]"
  computed (map generatePrime [4,6,8])
testHeron = do
  expected "[0.0]"
  computed (heron 0)
  expected "[1.0]"
  computed (heron 1)
  expected "[2.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899,1.414213562373095]"
  computed (heron 2)

expected e = putStrLn ("expected: " ++ e)
computed c = putStrLn ("computed: " ++ show c)


testFibGen :: String -> (Integer -> Integer) -> IO ()
testFibGen s f = mapM_ test (zip [0..] xs)
  where xs = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
        test (n, m) =
           do putStr ("Testing " ++ s ++ " " ++ show n ++ " == " ++ show m ++ "... ")
              hFlush stdout
              let m' = f n
              if m' == m then
                putStrLn "OK"
              else
                putStrLn ("WRONG (got " ++ show m' ++ ")")

testFib = testFibGen "fib" fib
testFib' = testFibGen "fib'" fib'
testFibFast = testFibGen "fibFast" fibFast
