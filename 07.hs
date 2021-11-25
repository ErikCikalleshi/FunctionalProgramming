-- Exercise 1

data Rat = Rat Integer Integer

normaliseRat :: Rat -> Rat
normaliseRat (Rat a b) 
  | b < 0 = Rat((-a) `div` x)((-b) `div` x) --wenn Nenner negativ ist
  | otherwise = Rat(a `div` x)(b `div` x) -- Rat wird so gerprintet wie die Eingaben
  where 
    x = gcd a b --größter gemeinsamer Teiler

instance Eq Rat where --beide Brüche werden normalisiert ung verglichen
  (==) (Rat a1 b1) (Rat a2 b2) = (a_1 == a_2) && (b_1 == b_2) where 
    (Rat a_1 b_1) = normaliseRat(Rat a1 b1)
    (Rat a_2 b_2) = normaliseRat(Rat a2 b2)

instance Ord Rat where
  (Rat a1 b1) <= (Rat a2 b2) = a1*b2 <= b1*a2
  (Rat a1 b1) >= (Rat a2 b2) = a1*b2 >= b1*a2
           
instance Show Rat where
   show (Rat a b) 
     | d == 1 = show(c) -- wenn der Nenner 1 ist, dann wird nur der Zähler c von der Normalisierten Form geprintet
     | otherwise = show c ++ "/" ++ show d
    where 
      (Rat c d) = normaliseRat(Rat a b)
      
instance Num Rat where
  (+) (Rat a b) (Rat c d) = normaliseRat(Rat (a * d + b * c) (b * d)) 
  (*) (Rat a b) (Rat c d) = normaliseRat(Rat (a * c) (b * d))
  (abs) (Rat a b) = Rat (abs a) (abs b) 
  (signum) (Rat a b) = Rat (signum (a*b)) (1)
  (negate) (Rat a b) = (Rat (-a) b)
  (fromInteger) (x) = Rat (x) 1



-- Exercise 2

data Unit = ML | G | PC deriving Show

data Ingredient = Ingredient String Float Unit

createIngredient :: String -> Float -> Unit -> Ingredient
createIngredient n a u = Ingredient n a u 

instance Show Ingredient where
  show price@(Ingredient n a u) = show a ++ " " ++ show u ++ " of " ++ n ++ ", costs: " ++ show (getPrice price) ++ " EUR"

class Price a where
  getPrice :: a -> Float


instance Price Ingredient where
  getPrice (Ingredient _ a u) = case u of
    ML -> a * 0.0012
    PC -> a * 0.75
    G -> a * 0.00095

data Recipe = Recipe [Ingredient]

createRecipe :: [Ingredient] -> Recipe
createRecipe x = Recipe x 

instance Price Recipe where
    getPrice (Recipe x) = case x of
      [] -> 0
      (x:xs) -> getPrice x + getPrice(Recipe xs)

instance Show Recipe where
  show recipe = helper recipe ++ "Price of the Recipe: " ++ show(getPrice recipe) ++ " EUR" where
   helper (Recipe (x:xs)) = show x ++ " - " ++ helper(Recipe xs)
   helper (Recipe []) = ""


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

