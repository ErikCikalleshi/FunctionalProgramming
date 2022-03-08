module Main (main) where

import Logic
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- avoid buffering problems
  putStrLn "Welcome to Connect Four"
  putStr "(n)ew game or (l)oad game:"
  res <- getLine

  --load Game
  if (res == "l")
    then do
      contents <- readFile "connect4.txt"
      --[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,2,1,2,0,0,0,0]
      print $ map (read :: String -> Tile) (map (: []) (concat (lines contents)))
      let values = map (read :: String -> Tile) (map (: []) (concat (lines contents)))
      game ((initOnLoad 1) values)
    else game (initState 1)

game state = do
  putStrLn $ showState state
  case winningPlayer state of
    Just player -> do
      putStrLn $ showPlayer player ++ " wins!\nDo you want to play again? (y = yes | n = no)"
      question <- getLine
      --restart
      if (question == "y")
        then game (initState (player))
        else -- exit
          return ()
    Nothing ->
      let moves = validMoves state
       in if null moves
            then do
              putStrLn "Game ends in draw."
            else --game (initState (otherplayer player))
            do
              hSetBuffering stdout NoBuffering -- avoid buffering problems
              let msg = "Choose one of " ++ show moves ++ "or (s)ave : "
              putStr msg
              fun moves state

fun moves state = do
  moveStr <- getLine
  -- Save
  if (moveStr == "s")
    then writeFile "connect4.txt" (tilesToString state) --write in File
    else -- cannot give two digits and the digits must be < 0 and less then >6

      if ((length moveStr) /= 1 || (moveStr < "0" && moveStr > "6"))
        then falseMove state moves moveStr
        else do
          -- execute the action
          let move = (read moveStr :: Move)
          game (dropTile move state)

falseMove state moves moveStr = do
  putStr $ moveStr ++ " is not a valid move, try again: "
  fun moves state -- recall fun;
