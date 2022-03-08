import Data.Char -- useful for Exercise 1

-- Exercise 1

{--

double x = x * 2
square x = x * x
add2times x y = x + double y
func x y = square x + add2times y x

Evaluate each of the following expressions step-by-step under the three evaluation strategies call-by-value,
call-by-name, and call-by-need.

a) add2times (5+2) 8
    call-by-value: add2times (5+2) 8 = add2times 7 8 = 7 + (double 8) = 7 + (8 * 2) = 7 + 16 = 23
    call-by-name: add2times (5+2) 8 = (5+2) + (double 8) = (Either the left side of the equation is executed or the right one) (5+2) + (8 * 2) = (5+2) + 16 = 7 + 16 = 23
    call-by-need: same as call by name, because the structer doesn't repeat itself

b) double (square 5)
    call-by-value: double (square 5) = double (5 * 5) = double 25 = 25 * 2 = 50
    call-by-name: double (square 5) = (square 5) * 2 = (5 * 5) * 2 = 25 * 2 = 50
    call-by-need: same as call by name, because the structer doesn't repeat itself

c) func (2+2) 4
    call-by-value: func (2+2) 4 = func 4 4 = square 4 + add2times 4 4 = (4 * 4) + (4 + double 4) =  (4 * 4) + (4 + 8) = 16 + 4 + 8 = 28
    call-by-name:  func (2+2) 4
                        = square (2 + 2) + add2times 4 (2 + 2)
                        = (we start to evaluate the right side) square (2 + 2) + (4 + (double (2 + 2)))
                        = square (2 + 2) + (4 + ((2 + 2) * 2))
                        = (2 + 2) * (2 + 2) + (4 + ((2 + 2) * 2))
                        = (2 + 2) * (2 + 2) + (4 + (4 * 2))
                        = (2 + 2) * (2 + 2) + (4 + 8)
                        = (2 + 2) * (2 + 2) + 12
                        = ((2 + 2) * 4) + 12
                        = (4 * 4) + 12
                        = 16 + 12
                        = 28
    call-by-need:  func (2+2) 4
                   = square (2 + 2) add2Times 4 (2 + 2)
                   = square (2 + 2) + 4 + double (2 + 2)
                   = square (2 + 2) + 4 + ((2 + 2) * 2)
                   = ((2 + 2) * (2 + 2)) + 4 + ((2 + 2) * 2)
                   = (4 * 4) + 4 + (4 * 2)
                   = (4 * 4) + 8
                   = 16 + 8
                   = 24

  2) a) squareList is guarded recursive 
     b) doubleTimes is tail recursive
     c) add2List linear
     d) tail recursion

--}

stringToUpperTail :: String -> String
stringToUpperTail x = aux "" x 
  where
    aux acc [] = acc
    aux acc (x : xs) = aux (acc ++ [toUpper x]) xs

stringToUpperGuarded :: String -> String
stringToUpperGuarded [] = []
stringToUpperGuarded (x:xs) = toUpper x : stringToUpperGuarded xs
-- map toUpper xs

-- Exercise 2
type Graph a = [(a, a)]

type RootedGraph a = (a, Graph a)

data Tree a = Node a [Tree a] deriving (Eq, Show)

graphFigure1 :: RootedGraph Int
graphFigure1 = (1, [(1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (3, 1), (4, 1)])

unwind :: Eq a => RootedGraph a -> Tree a
unwind (root, trees) = Node root [unwind (b, trees) | (a, b) <- trees, a == root]
-- unwind n = let res = snd n in Node (fst n) $ aux1 (rmdups (map fst res)) res
--   where
--     aux1 [] r = []
--     aux1 (y : ys) r = Node y (aux (map snd (filter (\(a, _) -> a == y) r))) : aux1 ys r
--       where
--         aux [] = []
--         aux (x : xs) = Node x [] : aux xs

-- rmdups [] = []
-- rmdups (x : xs) = x : let rem = filter (/= x) xs in rmdups rem

-- tree :: Tree Int
-- tree = Node 1 [Node 1 [Node 1 [], Node 2 [], Node 3 [], Node 4 []], Node 2 [Node 1 []], Node 3 [Node 1 []], Node 4 [Node 1 []]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node root x) = Node root []
prune n (Node root trees) = Node root [prune (n - 1) leaf | leaf <- trees]

narrow :: Int -> Tree a -> Tree a
narrow n (Node root trees) = Node root $ map (\x -> narrow n x) (take n trees)


mults :: Tree Integer
mults = Node 1 (aux 1) where
     aux y = [Node z (aux z) | z <- [y * x | x <- [2 .. ]]]
     
-- mults = unwind (1, [(x, x * y) | x <- [1 ..], y <- [2 .. ]])
-- mults = unwind (1, [(x, y) | x <- [1 ..], y <- [x * n | n <- [2 .. ]]])

{--
  narrow 4 $ prune 2 mults: 
        prune 2 mults -> takes 2 levels of the mults tree
        narrow 4 -> the maximum of nodes in each tree or subtree is 4

  narrow 1 mults:
        the maximum of nodes in each mults tree or subtree is 1

  prune 1 mults:
        the tree consists of two levels: root and all the natural number [1..]
--} 

-- Tests
firsts :: Tree a -> [a]
firsts (Node x []) = [x]
firsts (Node x (t : _)) = x : firsts t

tests = do
  test "to-upper-tail" "\"HELLO\"" (stringToUpperTail "Hello")
  test "to-upper-guarded" "\"WORLD!\"" (stringToUpperGuarded "World!")
  test "narrow" "Node 1 [Node 2 []]" (narrow 1 $ Node 1 [Node 2 [], Node 3 []])
  test "prune+unwind" "Node 1 [Node 1 [Node 1 [],Node 2 [],Node 3 [],Node 4 []],Node 2 [Node 1 []],Node 3 [Node 1 []],Node 4 [Node 1 []]]" (prune 2 $ unwind graphFigure1)
  test "prune+unwind" "Node 1 []" (prune 0 $ unwind graphFigure1)
  test "mults" "[1,2,4,8,16,32,64,128,256,512]" (take 10 $ firsts mults)

test name e c = do
  putStr ("*** " ++ name ++ ": ")
  if show c == e
    then putStrLn "OK"
    else putStrLn ("ERROR; expected '" ++ e ++ "', but found '" ++ show c ++ "'")
