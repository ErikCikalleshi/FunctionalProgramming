-- Exercise 1

data Rat = Rat Integer Integer

normaliseRat :: Rat -> Rat
normaliseRat (Rat a b) 
  | a < 0 && b < 0 = Rat((-a) `div` x)((-b) `div` x)
  | otherwise = Rat(a `div` x)(b `div` x)
  where 
    x = gcd a b

instance Eq Rat where
  -- (Rat a1 b1) == (Rat a2 b2) = a1 == a2 && b1 == b2 -- 4/5 == 4/5
  -- (Rat a1 b1) == normaliseRat(Rat a1 b1) = true --10/15 == 5/3
  (==) (Rat a1 b1) (Rat a2 b2) = (a_1 == a_2) && (b_1 == b_2) where 
    (Rat a_1 b_1) = normaliseRat(Rat a1 b1)
    (Rat a_2 b_2) = normaliseRat(Rat a2 b2)

instance Ord Rat where
  (Rat a1 b1) <= (Rat a2 b2) = a1*b2 <= b1*a2
  (Rat a1 b1) >= (Rat a2 b2) = a1*b2 >= b1*a2
           
instance Show Rat where
   show (Rat a b)
     | b == 1 && a > 0 = show(a)
     | b == -1 && a > 0 = show((- a))
     | a < 0 && b > 0 = show ((-a)) ++ "/" ++ show b
     | a > 0 && b < 0 = show ((-a)) ++ "/" ++ show ((-b))
     | a < b = show a ++ "/" ++ show b
   --  | a < 0 && b < 0  && b /= -1 = show(normaliseRat(Rat (a * (-1)) (b * (-1)) ))
    -- | otherwise = show(Rat a b)
      
instance Num Rat where


-- Exercise 2

data Unit = ML | G | PC deriving Show

data Ingredient = String | Float Unit

createIngredient :: String -> Float -> Unit -> Ingredient
createIngredient n a u = Ingredient n a u -- specify as soon as Ingredient has been defined

instance Show Ingredient where
  show (Ingredient n a u) = show a ++ " " ++ show u ++ " of " ++ show n


-- remove the following line, as soon as 
-- you define the class Price
getPrice x = (undefined :: Float)

data Recipe 

createRecipe :: [Ingredient] -> Recipe
createRecipe = undefined -- specify as soon as Recipe has been defined

instance Show Recipe where


-- Tests

expected e = putStrLn ("expected: " ++ e)
computed c = putStrLn ("computed: " ++ show c)

testRat = do
  expected "True"
  computed (case (normaliseRat (Rat (-1) (-2)), normaliseRat (Rat 2 4)) of
    (Rat n1 d1, Rat n2 d2) -> (n1,d1) == (n2,d2))

  expected "[True,False]"
  computed [Rat 3 5 == Rat (-9) (-15), Rat 3 5 == Rat 5 3]

  expected "[True,False,True]"
  computed [Rat 3 5 < Rat 3 4, Rat 2 4 < Rat 2 4, Rat (-5) 3 < Rat 10 1]

  expected "[3,4/5,-1/2]"
  computed [Rat 3 1, Rat 4 5, Rat 1 (-2)]

  expected "[1/2,7/12,3/4,-1]"
  computed [3 * Rat 1 6, Rat 1 3 + Rat 1 4, abs (Rat (-3) 4), signum (Rat (-3) 4)]


  putStrLn "some simple calculation: compute the sum 1/1 + 1/2 + 1/3 + ... + 1/100, i.e., sum [ Rat 1 n | n <- [1..100] ]"
  expected "14466636279520351160221518043104131447711/2788815009188499086581352357412492142272"
  computed (sum [ Rat 1 n | n <- [1..100] ])

ing1 = createIngredient "Milk" 200 ML
ing2 = createIngredient "Sugar" 200 G
ing3 = createIngredient "Egg" 3 PC
recipe = createRecipe [ing1, ing2, ing3]

testIng = do
  expected "0.19"
  computed (getPrice ing2)
  expected "0.4275"
  computed (getPrice (createIngredient "Test" 450 G))
  expected (show "200.0 ML of Milk, cost: 0.24 EUR")
  computed (show ing1)
  expected (show "3.0 PC of Egg, cost: 2.25 EUR")
  computed (show ing3)

testRecipe = do
  expected "200.0 ML of Milk, cost: 0.24 EUR - 200.0 G of Sugar, cost: 0.19 EUR - 3.0 PC of Egg, cost: 2.25 EUR - Price of the Recipe: 2.68 EUR"
  computed recipe
  expected "2.68"
  computed (getPrice recipe)

