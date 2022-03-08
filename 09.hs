import Data.Char -- useful for Caesar exercise
import Data.Ratio -- for Bernoulli exercise
import Data.List -- for the tests

{- Caesar cipher-}

shift :: Int -> Char -> Char
-- shift a character c by n slots to the right
shift n c | isLower c = toEnum ((((fromEnum c - 97) + n) `mod` 26) + 97) 
          | otherwise = toLower c

encode :: Int -> String -> String
encode n [] = []
--encode n (x:xs) = if(x >= 'a' && x <= 'z') then (shift n (toLower x)) : encode n xs else []
encode n (x:xs) = shift n (toLower x) : encode n xs

freqList = [8.2, 1.5, 2.8, 4.3, 13, 2.2, 2, 6.1, 7, 0.15, 0.77, 4, 2.4, 6.7,
            7.5, 1.9, 0.095, 6, 6.3, 9.1, 2.8, 0.98, 2.4, 0.15, 2, 0.074]

count :: Char -> String -> Int
count c s = length (filter (== c) s)

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

freqs :: String -> [Float]
freqs s = helper (map (\x -> count x s) ['a' .. 'z']) (length s)
  where 
    helper :: [Int] -> Int -> [Float] 
    helper [] _ = []
    helper (x:xs) len = percent x len : helper xs len
--[percent (count x s) l | x <- ['a' .. 'z']] where 
    -- l = length s
chisqr :: [Float] -> [Float] -> Float
chisqr [] _ = 0.0
chisqr _ [] = 0.0
chisqr (os_i: os) (es_i:es) = (((os_i - es_i)^2)/es_i) + chisqr os es
-- = sum [(x-y)**2 / y| (x, y) <- zip os es]
rotate :: Int -> [a] -> [a]
rotate n x = drop n x ++ take n x

positions :: Eq a => a -> [a] -> [Int]
positions n xs = map (\(x,y) -> y) (filter ((==n) . fst) (zip xs [0 ..]))
-- [p | (p,s) <- zip [0..l]xs, n == s] where 
      --l = length xs - 1


crack :: String -> String
crack s = map (\x -> shift (negate (round (minimum( map (\x -> chisqr (rotate x (freqs s)) freqList) [0..25])))) x) s
--c s = round (minimum( map (\x -> chisqr (rotate x (freqs s)) freqList) [0..25] ))
encString = "rkcuovv sc pex"

{--Bernoulli numbers--}

fact :: Integer -> Integer
fact x 
    |x >= 1 = x * fact (x-1)
    |otherwise = 1
-- fact 0 = 1
-- fact a = product [1..a]

binom :: Integer -> Integer -> Integer
binom x y 
    | x >= 1 && y >= 1 = div (fact x)  ((fact y)  * (fact(x - y)))
    |otherwise = 1

bernoulli :: Integer -> Rational
bernoulli n 
    | n == 0 = 1
    | otherwise = foldr(+) 0 (map (\x -> toRational(binom n x) * ((bernoulli x) / toRational(x - n - 1))) [0..(n-1)])
    -- num == 0 = 1
    -- otherwise sum[toRational(binom num k) * (bernoulli k) / toRational(k-num-1) | k <-[0..(num-1)]]
    -- | otherwise = foldr (+) 0 [toRational(binom n k) * ((bernoulli k) / toRational(k - n - 1)) | k <- [0..(n-1)]]
-- struggles at [0..16/17]

bernoullis :: Integer -> [Rational]
bernoullis n = map bernoulli [0..n]
-- bernoullis = (map bernoulli [0..] !!)
bernoulliss = (map bernoulli [0..] !!)



check1 :: Integer -> Bool
check1 i = all (== True) (map (\x -> if(bernoulli x) == 0 then True else False) [k | k <- [3..(3+i)], odd k])

check2 :: Integer -> Bool
check2 i = if(fromIntegral(length (filter (== True) ((map (\x -> if(bernoulli x) > 0 then True else False)) [k | k <- [2..(2*i)], even k]))) == (div i 2))
             then True else False

{-- Tests  --}

-- The following should print the first eight rows of Pascal's triangle - try it out!
pascalTriangle = putStrLn $ unlines $ map (\n -> spaces ((m - n) * 2) ++ (intercalate " " $ map (pad 3 . show . binom n) [0..n])) [0..m]
  where m = 8
        spaces n = replicate (fromIntegral n) ' '
        pad n s = let l = length s in if l < n then s ++ spaces (n - l) else s

tests = do
  test "1.1  " "\"mjwj nx fs jcfruqj.\"" (encode 5 "here is an example.")
  test "1.2a " "2" (count 'e' "example")
  test "1.2a'" "33.333336" (percent 1 3)
  test "1.2b " "[10.0,20.0,30.000002,40.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]" (freqs "abbcccdddd")
  test "1.2c " "25.0" (chisqr [50,50,0] [40,40,20])
  test "1.2d " "[4,5,1,2,3]" (rotate 3 [1,2,3,4,5])
  test "1.2d'" "[0,2,3]" (positions 3 [3,1,3,3])
  test "1.3  " "\"test\"" (crack (encode 10 "test"))
  test "2.1a " "[1,1,2,6,24,120]" (map fact [0..5])
  test "2.1b " "[1,5,10,10,5,1]" [binom 5 k | k <- [0..5]]
  test "2.2  " "[1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42]" (map bernoulli [0..6])
  test "2.3  " "[1 % 1,(-1) % 2,1 % 6,0 % 1,(-1) % 30,0 % 1,1 % 42]" (bernoullis 6)
  test "2.4a " "True" (check1 10)
  test "2.4b " "True" (check2 10)

test name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e then putStrLn "OK"
  else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")
