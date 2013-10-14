{-
  Queens.hs
  
  Solves the Eight Queens problem
-}

import Data.Array.IArray

main =
  do
  putStr "Enter y value to start board at: "
  yStr <- getLine
  let y = read yStr::Integer
  let (solved, board) = solveBoard y
  if solved
  then do putStrLn "Solved!" 
  else do putStrLn "No Solution found"
  putStr (showBoard board)

baseBoard :: Array (Integer, Integer) Char
baseBoard = 
  array ((0,0), (7,7)) [((i,j), ' ') | i <- [0..7], j <- [0..7]]

-- calls solve to solve the board, used in main. Allows for different start positions
solveBoard y = solve 0 y baseBoard

-- main recursive solve function
solve x 8 board = (False, board)
solve 7 y board = 
  if board ! (7,y) /= 'x'
  then (True, (board // [((7,y), 'Q')]))
  else solve 7 (y+1) board
solve x y board =
  if isSafe
  then if done
       then (done, new_board)
       else solve x (y+1) board
  else solve x (y+1) board
  where
    (done, new_board) = solve (x+1) 0 (markBoard x y)
    isSafe = board ! (x,y) /= 'x' 
    -- marks the board with unsafe spots and current queen positions
    -- only marks columns >= x
    markBoard 7 y = board // [((7,y), 'Q')]
    markBoard x y = board // (((x,y), 'Q') : vertical ++ horizontal ++ diagonals)
    xs = [(x+1) .. 7]
    vertical = [((x,j), 'x') | j <- [0..7], (j,x) /= (y,x)]
    horizontal = [((i,y), 'x') | i <- xs]
    diagonals = [(t, 'x') | t <- zip xs (reverse [0..(y-1)]) ] ++
                [(t, 'x') | t <- zip xs [(y+1) .. 7]]

-- converts the board to a better string for viewing
showBoard board = aux $ assocs board
  where
    ((_,_), (hRange, wRange)) = bounds board 
    aux board = concat $ map (\((_,x), e) ->
                               if x == wRange
                               then "[" ++ (show e) ++ "]" ++ "\n" 
                               else "[" ++ (show e) ++ "]")  board
