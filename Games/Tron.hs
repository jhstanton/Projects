{-
  Tron.hs
  Simple Tron clone using the Gloss library.

  Jim Stanton

  The variants in the game AI are based on an infinite sequence
  of pseudo random numbers that is 'dropped' by several functions
  in the main loop, including whenever the player presses a key.
  I thought this was an interesting way to add some legitimate randomness
  while remaining within pure functions.
-}

import Data.Monoid
import Prelude hiding (Either(..))
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as Game
import Data.Maybe(fromJust)
import System.Random

main = do
  Game.play
    (InWindow "Tron" (boardWidth, boardHeight) (1,1))
    black
    40
    (player1, player2, False, False, False, False, aiRands, 0)
    (drawBoard . drawText)
    input
    (step )
    
-- game constants
boardWidth  = 800
boardHeight = 600
bikeWidth   = 4 :: Int
quarter = div boardWidth 4
mid     = div boardHeight 2
player2 = Bike blue North (-quarter, 0)  []
player1 = Bike red  South  (quarter , 0) []

-- random values and random value producer
-- this is used only when generating next ai move. 2:23 is the time in the morning I was writing this,
-- which seems as good a seed as any
aiRands          = randomRs (0,2) (mkStdGen 223) 
nextRands (r:rs) = drop r rs  
newRWorld (bike1, bike2, collide1, collide2, started, single, aiRands, evaded) = 
  (bike1, bike2, collide1, collide2, started, single, nextRands aiRands, evaded)

type Coord = (Int, Int)
{-this is the form of (player1, player2, 
                       player1collision, player2collision, 
                       gamestarted, singleplayer, aiRands, lastAIEvade) -}
type World = (Bike, Bike, Bool, Bool, Bool, Bool, [Int], Int)

data Bike = Bike Color Dir Coord [Coord]
data Dir  = North | South | East | West deriving (Eq)


drawBoard :: (World, Maybe Picture) -> Picture
drawBoard ((Bike color1 _ pos1 coords1, Bike color2 _ pos2 coords2, _, _, started, _, _, _), mPic) =
  if started 
  then (bk1image <> trail1 <> bk2image <> trail2) 
  else fromJust mPic where
    trail1 = buildTrail color1 coords1
    trail2 = buildTrail color2 coords2
    buildTrail :: Color -> [Coord] -> Picture
    buildTrail bkcolor coords = mconcat $ map (\(x,y) -> rect bkcolor (fromIntegral x, fromIntegral y)) coords
    bk1image  = paintbike color1 pos1
    bk2image  = paintbike color2 pos2
    paintbike bkcolor (x,y) = rect bkcolor (fromIntegral x,fromIntegral y)
    bwFloat = fromIntegral bikeWidth
    rect bkcolor (x,y) = 
      Translate x y $ Color bkcolor $ (polygon [(0, 0), 
                                                  (bwFloat, 0),
                                                  (bwFloat, bwFloat),
                                                  (0, bwFloat)])
      
-- handles menus and messages, returns world to pass to drawboard
drawText :: World -> (World, Maybe Picture)
drawText world =  
  case world of
    (_, _, True, True, _ , _, _, _)      -> (world, Just $ (lossMessage "Double KO!" ) <> startMessage)
    (_, _, True, False,_ , _, _, _)      -> (world, Just $ (lossMessage "Red Loses!" ) <> startMessage)
    (_, _, False, True, _, _, _, _)      -> (world, Just $ (lossMessage "Blue Loses!") <> startMessage)
    (_, _, False, False, False, _, _, _) -> (world, Just startMessage)
    otherwise                         -> (world, Nothing)
    where
      startMessage  = Translate (1.5*x) 0  $ Color red $ Scale 0.2 0.2 $ Text "Press 's' for single player, 'm' for multiplayer"
      lossMessage s = Translate x ((fromIntegral mid) / 2) $ Color red $ Scale 0.3 0.3 $ Text s 
      x             = fromIntegral (-quarter)
      
-- sets a single bike one step forward in the current direction
move :: Bike -> Bike
move (Bike color dir c@(x,y) coords) =
  case dir of
    North -> Bike color dir (x, y+bikeWidth) new_coords
    South -> Bike color dir (x, y-bikeWidth) new_coords
    East  -> Bike color dir (x+bikeWidth, y) new_coords
    West  -> Bike color dir (x-bikeWidth, y) new_coords 
    where
      new_coords = c : coords
      
input :: Event -> World -> World
-- player 1 input
input 
  (EventKey (Char k) Down _ _) 
  w@( bike1 , bike2@(Bike bkcolor dir pos coords), collide1, collide2, started, single, aiRands, evaded) = do
    case k of
      'w' -> (bike1, Bike bkcolor North pos coords, collide1, collide2, started, single, newRands, evaded)
      's' -> if started
             then (bike1, Bike bkcolor South pos coords, collide1, collide2, started, single, newRands, evaded)
             else (player1, player2, False, False, True, True, newRands, evaded) -- set up new single player game      
      'a' -> (bike1, Bike bkcolor West pos coords, collide1, collide2, started, single, newRands, evaded)
      'd' -> (bike1, Bike bkcolor East pos coords, collide1, collide2, started, single, newRands, evaded)
      'm' -> if not started
             then (player1, player2, False, False, True, False, newRands, evaded)   
             else (bike1, bike2, collide1, collide2, started, single, newRands, evaded)    
      _   -> (bike1, bike2, collide1, collide2, started, single, newRands, evaded) 
      where
        newRands = nextRands aiRands

  
-- player 2 inputs 
input 
  (EventKey (SpecialKey k) Down _ _ )
  (bike1@(Bike bkcolor dir pos coords), bike2, collide1, collide2, started, False, aiRands, evaded) = do
    case k of
      KeyUp    -> (Bike bkcolor North pos coords, bike2, collide1, collide2, started, False, newRands, evaded)
      KeyDown  -> (Bike bkcolor South pos coords, bike2, collide1, collide2, started, False, newRands, evaded)
      KeyLeft  -> (Bike bkcolor West  pos coords, bike2, collide1, collide2, started, False, newRands, evaded)
      KeyRight -> (Bike bkcolor East  pos coords, bike2, collide1, collide2, started, False, newRands, evaded)
      _        -> (bike1, bike2, False, False, started, False, newRands, evaded) 
      where
        newRands = nextRands aiRands
      
-- otherwise      
input e w = newRWorld w

-- main function to step through each frame
step :: Float -> World -> World
step _ w@(_, _, _, _, True , single, aiRands, _) = 
  let (bike1, bike2, collided1, collided2, _, _, _, evaded) = (detectCollision . runAI) w 
      (aibike, _, _, _, _, _, newerRands, _) = runAI w in 
  if collided1 || collided2
  then (bike1, bike2, collided1, collided2, False, single, aiRands, 6) -- stop the game, save state
  else if single
       then (move aibike, move bike2, collided1, collided2, True, single, nextRands newerRands, evaded)   
       else (move bike1, move bike2, collided1, collided2, True, single, nextRands newerRands, evaded)
                         
step _ (   _ ,    _ , collide1, collide2, False, single, aiRands, evaded) = 
  (player1, player2, collide1, collide2, False, single, aiRands, evaded)

-- detect if either bike has collided in this frame
detectCollision :: World -> World
detectCollision ( bike1@(Bike color1 _ pos1 coords1), 
                  bike2@(Bike color2 _ pos2 coords2), _, _, started, single, rs, evaded) =
  (bike1, bike2, hitAny pos1 pos2, hitAny pos2 pos1, started, single, rs, evaded) where
    hitAny p1 p2 = checkBounds p1 || (elem p1 (p2 : (coords1 ++ coords2)))
    -- check that bike is remaining on board
    checkBounds (x,y) = y >= mid || y <= -mid || x >= 2*quarter || x <= (-2)*quarter

-- Simple single player ai function

runAI :: World -> World
runAI (bike1@(Bike bkcolor dir pos@(x,y) aiCoords), 
       bike2@(Bike _ _ playerPos playerCoords), 
       False, False, started, True, (r:rs), lastEvade) = 
  if not $ null allMoves
  then (Bike bkcolor (fst $ head allMoves) pos aiCoords, bike2, False, False, started, True, nextRands rs, evadeCount)
  else (bike1, bike2, False, False, started, True, nextRands rs, evadeCount)
    where
      allMoves = gatherMoves bike1 bike2
      oldCoords = pos : playerPos : (aiCoords ++ playerCoords)
      (Bike _ _ nextMove _ )= move bike1
      willCrash (x', y') = 
        let (_ , _ , aiCollide, playerCollide , _, _, _, _) = 
              detectCollision (Bike bkcolor dir (x', y') (pos:aiCoords), Bike red South playerPos playerCoords, 
                               False, False, True, True, rs, evadeCount) in  (aiCollide, playerCollide)
      gatherMoves :: Bike -> Bike -> [(Dir, (Int, Int))]
      gatherMoves (Bike _ _ (x,y) aicoords) (Bike _ _ playerPos playerCoords) =
        if lastEvade + r == 7 && lastEvade > 1 
        then filterMoves potentialAIMoves
        else filterMoves $ (dir, nextMove) : potentialAIMoves     
      -- evadeCount is used to help make a natural turn when confronted with situations calling for evasive maneuvers        
      evadeCount = if lastEvade > 20 && (not . null) allMoves && dir /= (fst $  head allMoves)
                   then 0
                   else lastEvade + 1     
      filterMoves = filter (not . fst . willCrash . snd)        
      potentialAIMoves = drop r $ zip [East, South, West, North] 
                         [(x+bikeWidth, y), (x, y-bikeWidth), (x-bikeWidth, y), (x, y+bikeWidth)]
      potentialPlayerMoves = foldl (\(b:bs) _ -> (move b) : (b:bs)) [move bike2] [1..10] 

runAI w@(_, _, _, _, _, _, _, _) = w        
