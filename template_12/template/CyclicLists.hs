-- Exercise 1
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = []
merge [] ys = []
merge a@(x:xs) b@(y:ys)
  | x == y = x : merge xs ys
  | x < y = x : merge xs b
  | otherwise = y : merge a ys


sNumbers :: [Integer]
sNumbers = 1 : merge (map (3*) sNumbers) (merge (map (7*) sNumbers) (map (11*) sNumbers))

sNum :: Integer -> Integer
sNum x = aux 1 x where
   aux acc x
    | isSpecial acc = if (x /= 0) then aux (acc + 1) (x - 1) else acc
    | otherwise = aux (acc + 1) x


isSpecial n = 1 == aux 11 (aux 7 (aux 3 n)) where
  aux toDiv n = if(mod n toDiv /= 0) then n else aux toDiv (div n toDiv)

-- Tests

tests = do
  test "merge" "[1,18,19,150,200,300,301]" (merge [1, 18, 200, 300, 301] [19, 150, 200, 300, 301])
  test "sNumbers" "[1,3,7,9,11,21,27,33,49,63,77,81,99,121,147,189,231,243,297,343]" (take 20 sNumbers)
  test "sNum" "[1,3,7,9,11,21,27,33,49,63,77,81,99,121,147,189,231,243,297,343]" (map sNum [0 .. 19])

test name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e
    then putStrLn "OK"
    else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")
