module Logic(State, Move, Player, Tile,
  initState, showPlayer, showState,
  winningPlayer, validMoves, dropTile, tilesToString, initOnLoad) where
 -- added tilesToString and initOnLoad

type Tile   = Int   -- 0, 1, or 2
type Player = Int   -- 1 and 2
type Move   = Int   -- column number
data State = State Player [[Tile]]  -- list of rows

empty :: Tile
empty = 0

numRows, numCols :: Int
numRows = 6
numCols = 7

startPlayer :: Int -> Player
startPlayer y = otherPlayer (y)

--Input: replicate 3 5 Output: [5,5,5]
initState :: Int -> State
initState x = State (startPlayer x) (replicate numRows (replicate numCols empty))

-- Convert args to State
initOnLoad :: Int -> [Tile] -> State
initOnLoad x y = State(startPlayer x) (helper y)

-- helper [1,2,3,4,5,6,7,8,9,10,11,12,1,2] = [[1,2,3,4,5,6,7],[8,9,10,11,12,1,2]]
helper [] = []
helper xs = take 7 xs : helper (drop 7 xs) --take the first 7 and put them in an list

validMoves :: State -> [Move]
validMoves (State _ rows) =
  map fst . filter ((== empty) . snd) . zip [0 .. numCols - 1] $ head rows

showPlayer :: Player -> String
showPlayer 1 = "X"
showPlayer 2 = "O"

showTile :: Tile -> Char
showTile t = if t == empty then '.' else head $ showPlayer t

-- print the Field when loaded
showField :: Tile -> Char
showField t = if (t == empty) then '0' else if(t == 1) then '1' else '2'

--Input: unlines ["aa","bb","cc","dd","ee"] Output: "aa\nbb\ncc\ndd\nee\n"

showState :: State -> String
showState (State player rows) = unlines $
    map (head . show) [0 .. numCols - 1] :
    map (map showTile) rows
     ++ ["\nPlayer " ++ showPlayer player ++ " to go"]

-- new added
tilesToString :: State -> String
tilesToString (State player rows) = unlines $     
    map (map showField) rows 

otherPlayer :: Player -> Player
otherPlayer = (3 -)

dropTile :: Move -> State -> State
dropTile col (State player rows) = State
  (otherPlayer player) 
  (reverse $ dropAux $ reverse rows)
    where
      dropAux (row : rows) =
        case splitAt col row of
         (first, t : last) ->
           if t == empty 
             then (first ++ player : last) : rows
             else row : dropAux rows



winningRow :: Player -> [Tile] -> Bool
winningRow player [] = False
winningRow player row = take 4 row == replicate 4 player
  || winningRow player (tail row)


diags :: [[a]] -> [[a]]
diags matrix = lowerDiags matrix ++ upperDiags matrix
    where lowerDiags = reverse . transpose . zipWith drop [1..]
          upperDiags = transpose . zipWith drop [0..] . transpose

transpose ([]: _) = []
transpose x = getHead x: transpose (getTail x)


getHead [] = []
getHead ([]:ys) = getHead ys
getHead ((x:_):ys) = x : getHead (ys)

getTail [] = []
getTail ([]:ys) = getTail ys
getTail (x:xs) = if ((length x) > 1) then drop 1 x : getTail (xs) else [] : getTail(xs)

last' [x] = x
last' (_:xs) = last xs

winningPlayer :: State -> Maybe Player
winningPlayer (State player rows) =
  let prevPlayer = otherPlayer player
      longRows = rows ++ transpose rows ++ diags rows ++ diags (reverse rows)
    in if any (winningRow prevPlayer) longRows
      then Just prevPlayer
      else Nothing


