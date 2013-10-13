{-
  KnightsTour.hs

  A knights tour is an attempt to have knight move along a rectangular
  board in such a way that it touches each square only once. 
  This can be found for any board size m x n or greater, where 
  m = 5 and n > 5.

  In this implementation the board contains when each square was touched starting
  from move 0.

-}
module Main (main) where
import Data.Array.IArray


main = 
  do
  putStr "Enter board width, height, start x and start y positions separated by spaces:"
  paramStr <- getLine
  let width : height : x : y : _ = map (\x-> read x::Int) $ words paramStr
  let (solved, board) = solveBoard width height x y
  if solved
  then do putStrLn "Success! Board arrangement found: "
  else do putStrLn "Failure! "  
  putStr $ showBoard board
  
  

-- every move possible for a knight to make
allMoves = [(-2,-1), (-2,1), (-1,2), (1,2),
             (2, 1), (2,-1), (1,-2), (-1,-2)]

-- builds a base board of -1s from an array
buildBoard :: (Enum t, Enum t1, Num t, Num t1, Num e, Ix t1, Ix t) =>
               t1 -> t -> Array (t, t1) e
buildBoard width height = 
  array ((0, 0), (height-1, width-1)) [((i,j), -1) | i <- [0..height-1], j <- [0..width-1]]

-- calls solve
solveBoard width height x y = 
  solve x y 0 allMoves $ buildBoard width height

-- returns a tuple (bool, array), where the bool is whether or not the 
-- attempt worked, and the board corresponds with that answer. 
solve _ _ _ [] board = (False, board)
solve x y movenum ((xmove,ymove):ms) board = 
  if isValid x y 
  then if movenum == (xRange+1) * (yRange+1) - 1
       then (True, board // [((y,x), movenum)])
       else if done
            then (done, new_board)
            else solve x y movenum ms board
  else (False, board)
  where 
    ((_,_), (yRange, xRange)) = bounds board
    nextx = x + xmove
    nexty = y + ymove
    (done, new_board) = solve nextx nexty (movenum+1) allMoves (board // [((y,x), movenum)])
-- checks that a move is on the board and the space isn't taken
    isValid x y = 
      let ((_,_), (hRange, wRange)) = bounds board in
      x >= 0 && x <= wRange && y >= 0 && y <= hRange && board ! (y,x) == (-1)

-- converts the board to a better string for viewing
showBoard board = aux $ assocs board
  where
    ((_,_), (hRange, wRange)) = bounds board
    squareStr n = 
      if n < 10 && n >= 0
      then "[ " ++ (show n) ++ "]"
      else "[" ++ (show n) ++ "]" 
    aux board = concat $ map (\((_,x), e) ->
                               if x == wRange
                               then (squareStr e) ++ "\n" 
                               else squareStr e) board
