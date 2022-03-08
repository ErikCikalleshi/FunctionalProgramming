{-# LANGUAGE FlexibleContexts #-}
printList :: Show a => [a] -> String
printList [] = "{}"
printList xs = "{" ++ aux xs ++ "}" where
    aux (x: []) = show x ++ []
    aux (x:xs)  = show x ++ " - " ++ aux xs


data NewList a = ListConstr [a]

instance Show a => Show (NewList a) where
    --show(ListConstr xs) = drop 1 $ take (length (show xs)-1) (show xs)
    show(ListConstr xs) = tail $ init $ show xs

answerTask3 :: Bool
answerTask3 = False 

rotChar :: Char -> Char
rotChar x 
    | x == 'z' = 'a'
    | x >= 'a' && x <= 'z' = pred x
    | otherwise = x

nApply :: (a -> a) -> Int -> (a -> a)
nApply f 0 = id --(\x -> x)
nApply f n = f . nApply f (n - 1)

enc :: Int -> String -> String
enc 0 xs = xs
enc n xs = enc (n - 1) (map rotChar xs)


-- minList :: Ord b => (a -> b) -> [a] -> a
minList f xs = f $ minimum [f x| x <- xs]

type Item a = (a, Integer)   -- (item identifier, weight)

allTriples :: Ord a => [Item a] -> [((a, a, a), Integer)]
--  (i1, w1) <- xs, (i2, w2) <- xs, (i3, w3) <- xs, i1 > i2 && i2 > i3, let w = w1 + w2 + w3, w >= 149]

allTriples xs = [((x,y,z),w) | (x, w1) <- xs, (y,w2) <- xs, (z,w3) <- xs, x > y && y > z, let w = w1 + w2 + w3, w >= 149]

optimalCombination :: Ord a => [Item a] -> (a, a, a)
optimalCombination xs = undefined 

sumsq n = foldr (\x acc -> x*x + acc) 0 n

minlist :: Ord a => [a] -> a
minlist n = foldr(\x acc -> if (x < acc) then x else acc) (head n) n
minlist' n = foldl(\x acc -> if (x < acc) then x else acc) (head n) n

onefilter pred x xs
  | pred x = [x] 
  | otherwise  = xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred xs = foldr (onefilter pred) [] xs 

range' n = [y | (x,y)<-n]

compose' m n = [(x,z) | (x,y1)<-m, (y2,z)<-n, y1 == y2]

reflexive n = [(x1,x2) | (x1,y)<-n, (x2,y)<-n, x1 == x2]

dig2int n = foldr(\x acc -> x + 10 * acc) 0 n

suffs :: [a] -> [[a]]
suffs xs = foldr(\x acc -> (x : (head acc)) : acc) [[]] xs

{--
    "" , o -> ['o'] : [[]]
--}