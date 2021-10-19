plus1 x = x + 1
 
{-
   "0 :: Bool" --> not posible because type Bool expects only the type 'true' or 'false
  
   'head "test" :: Char' --> would work, but it doesn't make any sense for the 'plus1' function
 
   'hello':: String --> single quotation marks are used only for chars; to make this work we would use double quotation marks
 
   "plus1 :: Integer -> Integer" --> it defines that the function called "plus1" is taking as argument a Integer
       and it returns an Integer
 
-}


{-data Menu = -- name of type
  Menu       -- name of constructor
    Item
    Item
    Item
deriving Show

data Item = -- name of type
  Item      
    String    -- label name
    String   --link
deriving Show

itemFP = Item "Home" "http://cl-informatik.uibk.ac.at/teaching/ws21/fp"
itemOO = Item "Home" "http://cl-informatik.uibk.ac.at/teaching/ws21/fp"
checkMenu = Menu itemFP itemOO -}

data Item = -- name of type
  Item      
    String    -- label name
    String    --link
  deriving Show 

data Menu = -- name of type
    Empty 
  | Menu Item Menu      -- name of constructor
  deriving Show



itemFP = Item "Home" "http://cl-informatik.uibk.ac.at/teaching/ws21/fp"
itemFP2 = Item "Proseminar" "http://cl-informatik.uibk.ac.at/teaching/ws21/fp/exercises.php?lan=de"
itemOO = Item "Home" "https://lms.uibk.ac.at"
checkMenu = Menu itemOO(Menu itemFP(Menu itemFP2 Empty))
