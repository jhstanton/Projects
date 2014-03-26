{-
  Haskell Implementation of Conway's Game of Life
  Jim Stanton
  Compile this with -O2
-}
import Prelude hiding (Either(..))
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game
import Data.Array.IArray
import Data.List
import Data.Maybe(fromJust)

main = do
  Game.play 
    (InWindow "Game of Life" (displayWidth, displayHeight) (1,1))
    white
    10
    (grid, False, False, cellSize, (div gridWidth  2, div gridHeight  2))
    draw
    input
    step

data Living = Live | Dead deriving (Show, Eq)
flipLiving Live = Dead
flipLiving Dead = Live

displayWidth  = 800
displayHeight = 600
gridWidth     = 200
gridHeight    = 200
cellSize      = 20

grid :: Grid
grid = cleanGrid
cleanGrid :: Array Coord Living
cleanGrid = array ((1,1), (gridWidth, gridHeight)) [((i,j), Dead) | i <- [1..gridWidth], j <- [1..gridHeight]]

-- World type is form of    (Grid, running, paused in simulation, cell scale, center coord)
type World = (Grid, Bool, Bool, Int, Coord) 
type Grid  = Array Coord  Living 
type Coord = (Int,Int)

-- all basic rules that cause life
rules :: Living -> Int -> Living
rules Live n = if n < 2 || n > 3
               then Dead   
               else Live     
rules Dead 3 = Live
rules Dead _ = Dead

numSurrounding :: Grid -> Coord -> Int
numSurrounding grid (x,y) = 
  foldl (\acc (x',y') -> if validPosition (x',y') && (grid ! (x',y')) == Live  
                         then acc + 1    
                         else acc) 0 [(x+1,y), (x+1,y+1), (x,y+1),(x-1,y+1),
                                      (x-1,y), (x-1,y-1), (x,y-1),(x+1,y-1)]     

validPosition :: Coord -> Bool
validPosition (x,y) = (x>0 && x <=gridWidth) && (y>0 && y<=gridHeight)

-- main drawing function
draw :: World -> Picture
draw (grid, running, _, size, (x,y)) = 
  if running 
  then drawGrid 
  else drawGrid <> drawMenu
  where
    drawMenu = Translate (-(fromIntegral displayWidth)/2) (-(fromIntegral displayHeight)/2) $ Color red $ Scale 0.2 0.2 $ 
      Text "Click blocks to allocate 'living' cells, press 's' to start"
    drawGrid = foldl' drawCell blank $ assocs grid 
    drawCell :: Picture -> (Coord, Living) -> Picture
    drawCell p ((x',y') , live) = 
      case live of 
        Live -> p <> (trans blue (x', y') $ rectangleSolid fSize fSize)
        Dead -> p <> (trans black (x', y') $ rectangleWire fSize fSize)  
    trans :: Color -> Coord -> Picture -> Picture
    trans c (x',y') p = Translate ((fst $ getPos (x',y'))/ 2) ((snd $ getPos (x',y'))/ 2) $ Color c p   
    getPos (x',y') = (fromIntegral $ (x' - x) * 2 * size, fromIntegral $ (y' - y) * 2 * size)
    fSize = fromIntegral size

input :: Event -> World -> World
input (EventKey (Char k) Down _ _) w@(grid, started , paused, size, coord) =
  case k of
    'p'       -> if started 
                 then (grid, started, not paused, size, coord)
                 else (grid, started, paused, size, coord)
    'i'       -> (grid, started, paused, size + 10, coord)
    'o'       -> (grid, started, paused, size - 10, coord)
    's'       -> if started 
                 then (cleanGrid, not started, paused, size, coord) 
                 else (grid, not started, paused, size, coord)     
    otherwise -> w
input (EventKey (Char k) Down _ _) w@(grid, False, paused, size, coord) =
  case k of
    's'       -> (grid, True, False, size, coord)
    otherwise -> w
    
input (EventKey (SpecialKey k) Down _ _) w@(grid, running, paused, size, (x,y)) =    
  case k of 
    KeyUp     -> (grid, running, paused, size, (x, y+size))
    KeyDown   -> (grid, running, paused, size, (x, y-size))
    KeyLeft   -> (grid, running, paused, size, (x-size, y))
    KeyRight  -> (grid, running, paused, size, (x+size, y))
    otherwise -> w
    
-- this handles input for building the seed state
-- this should be updated to be more accurate when clicking
input (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) w@(grid, False, paused, size, (x,y)) = 
  let xPos' = div (truncate xPos) size + x 
      yPos' = div (truncate yPos) size + y 
      elems = find (\((a,b), _)-> a==xPos'&& b==yPos')$ assocs grid 
  in  
  case elems of
    (Just (coord, living)) -> (grid // [(coord, flipLiving living)], False, paused, size, (x,y))   
    Nothing                -> w

input _ w = w

step :: Float -> World -> World
step _ w@(_, False, _, _, _) = w
step _ w@(_, _, True, _, _) = w
step _ (grid, True, False, size, coord) = 
  (grid // (map (\(c, l) -> (c, rules l $ numSurrounding grid c)) $ assocs grid), True, False, size, coord)
step _ w  = w