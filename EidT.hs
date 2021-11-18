orOP :: Integer -> Integer -> Integer
orOP 0 0 = 0
orOP _ _ = 1

negOP :: Integer -> Integer
negOP 0 = 1
negOP 1 = 0

andOP :: Integer -> Integer -> Integer
andOP 1 1 = 1
andOP _ _ = 0


-- a) negOP( (0 `orOP` (1 `andOP` (0 `andOP` (negOP 1))) `andOP` 0) `orOP` (1 `orOP` 0) `andOP` ((negOP 1) `orOP` 0)) `andOP` ((negOP 1) `orOP` 1)
-- b) (negOP ( negOP ( negOP (negOP (0 `orOP` 1)))) `andOP` (0 `andOP` (negOP 1))) `orOP` ((negOP (negOP (0 `orOP` 1))) `andOP` (negOP (0 `andOP` (negOP 1))))
-- c)((1 `orOP` (negOP 1)) `andOP` (0 `orOP` (negOP ((negOP 0) `andOP` 1)))) `orOP` ((negOP(0 `andOP` (negOP 1)) `orOP` (((negOP 1) `andOP` 1) `andOP` (negOP (0 `andOP` 1)))))

