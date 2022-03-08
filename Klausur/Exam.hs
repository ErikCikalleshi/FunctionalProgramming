multiplex :: [Bool] -> [a] -> [b] -> [Either a b]
multiplex [] _ _ = []
multiplex _ [] _ = []
multiplex _ _ [] = []
multiplex (b:bs) (x:xs) (y:ys) = if(b == True) then (Left x) : multiplex bs xs ys else (Right y) : multiplex bs xs ys


pyramid :: [a] -> [[a]]
pyramid xs = aux xs 1 where
    aux [] acc = []
    aux xs acc = take acc xs : aux (drop acc xs) (acc + 1)

insAscending :: Ord a => [a] -> a -> Maybe [a]
insAscending xs n = if(elem n xs) then Nothing else (Just $ aux xs n) where
    aux [] n = []
    aux (x:[]) n = x : n : aux [] n
    aux (x:y:xs) n = if(x < n && n < y) then x : n : y : xs else x : aux (y:xs) n

