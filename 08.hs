import Data.List {- required to use groupBy -}

-- Exercise 1

{--
    fractional a => a -> a -> a
    div1 = (/) --Division Operation (Fractional a => a -> a -> a)
    div2 = (2 /) -- 2 divided with a Double (Double -> Double)
    div3 = (/ 2) -- Double divided with 2 (Double -> Double)
    eqTuple f = (\(x, y) -> f x == f y) -- In the Function eqTuple is callad a lambda Function that takes a x y and looks if f(x) equals f(y) =  Eq b => (a -> b) -> (a, a) ->  Bool
    eqTuple' f (x, y) = f x == f y -- Does the same like "eqTuple" but the function is already beeing called as an argument = Eq b => (a -> b) -> (a, a) ->  Bool

    Based on the definition both functions are equal, but the only difference is that anonymous lambda functions can not be recursively called
--}

div1 = (/)
div2 = (2 /)
div3 = (/ 2)
eqTuple f = (\(x, y) -> f x == f y)
eqTuple' f (x, y) = f x == f y

foo1 x y = y / x
foo2 x y = (\u v -> v / u) y x -- in the End the variable y and x is use to define the sequence of the input, therefore is foo2 equal to div1

-- Exercise 2
fan :: (a -> Bool) -> [a] -> [[a]]
-- fan p = groupBy (\x y -> p x == p y)
fan _ [] = []
-- fan p a@(x:xs) = if(p x) then [x] : fan p xs  else takeWhile (not . p) a : fan p (dropWhile (not . p) a)
fan p a@(x:xs) = if((len rs) /= 0) then rs : fan p rest else takeWhile(not . p) a : fan p (dropWhile(not . p) a)
                  where
                  (rs,rest) = span p a

len [] = 0 
len _ = 1             

splitOnNumbers :: String -> [String]
splitOnNumbers s = fan (\x -> x >= '0' && x <= '9') s

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p s = filter (\x -> not(p (head x))) (fan (not . p) s)



-- Exercise 3
dig2int :: [Integer] -> Integer
dig2int [] = 0 
dig2int (x:xs) = x + 10 * dig2int xs

dig2intFold :: [Integer] -> Integer
dig2intFold x = foldr (\x acc -> x + 10 * acc) 0 x
helper x = foldr (\x acc -> x + 10 * acc) 0 x

suffsFold :: [a] -> [[a]]
suffsFold [] = [[]]
suffsFold xs = foldr(\x y -> (x : head(y)) : y) [[]] xs

-- "hello" "" 

-- 'o' : [] : [[]] = "o" : [[]]
-- ["o", []]
-- 'l' : "o" : ["o", []]
-- "lo" : ["o",[]]

-- Tests
tests = do
  test "2.1a" "[]" (fan undefined [] :: [[Int]])
  test "2.1b" "[[1],[2],[3],[4],[5]]" (fan even [1..5])
  test "2.1c" "[\"T\",\"his is a \",\"T\",\"est\"]" (fan (== 'T') "This is a Test")
  test "2.2 " "[\"8\",\" out of \",\"10\",\" cats\"]" (splitOnNumbers "8 out of 10 cats")
  test "2.3 " "[\"Just\",\"some\",\"lines\"]" (splitBy (== '\n') "Just\nsome\nlines\n")
  test "3.1 " "512" (dig2intFold [2,1,5])
  test "3.2a" "[[1,2],[2],[]]" (suffsFold [1,2])
  test "3.2b" "[\"hello\",\"ello\",\"llo\",\"lo\",\"o\",\"\"]" (suffsFold "hello")

test name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e then putStrLn "OK"
  else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")
