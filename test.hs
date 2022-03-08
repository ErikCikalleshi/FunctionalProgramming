{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
not0 :: (Eq a, Num a) => a -> Bool
not0 = (/=0)

foo :: (Eq a, Num a) => [a] -> Bool
foo x = not0 (head (tail (tail x)))

foo' = not0 . head . tail . tail


number :: [a] -> [(Int,a)]
number = zip [1..]

--evenProdSum xs = let ys = number xs in sum $ map () [if(even x) then y else y - 1 |(x,y) <- ys]

greatest (x:xs) = aux x xs where
  aux acc [] = acc
  aux acc (x:xs) = if(x > acc) then aux x xs else aux acc xs  


average :: [Double] -> Double
average xs = aux 0.0 xs (fromIntegral $ length xs) where
  aux :: Double -> [Double] -> Double -> Double
  aux acc [] len = acc / len
  aux acc (x : xs) len = aux (acc + x) xs len

  

