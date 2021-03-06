{-
  Tron.hs
  Simple Tron clone using the Gloss library.

  Jim Stanton
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
    (World player1 player2 False False False False aiRands 0)
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
newRWorld w@(World _ _ _ _ _ _ aiRands _) = w {aiRs=nextRands aiRands}


type Coord = (Int, Int)
{-this is the form of (player1, player2, 
                       player1collision, player2collision, 
                       gamestarted, singleplayer, aiRands, lastAIEvade) -}
data World = World { p1bike::Bike
                   , p2bike::Bike
                   , p1collision::Bool
                   , p2collision::Bool
                   , gameStarted::Bool
                   , singlePlayer::Bool
                   , aiRs::[Int]
                   , lastAIEvade::Int}

data Bike = Bike { color::Color
                 , dir::Dir
                 , pos::Coord
                 , prevPos::[Coord]}
data Dir  = North | South | East | West deriving (Eq)


drawBoard :: (World, Maybe Picture) -> Picture
drawBoard ((World (Bike color1 _ pos1 coords1) (Bike color2 _ pos2 coords2) _ _ started _ _ _), mPic) =
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
    World _ _ True True _ _ _ _       -> (world, Just $ (lossMessage "Double KO!" ) <> startMessage)
    World _ _ True False _ _ _ _      -> (world, Just $ (lossMessage "Red Loses!" ) <> startMessage)
    World _ _ False True _ _ _ _      -> (world, Just $ (lossMessage "Blue Loses!") <> startMessage)
    World _ _ False False False _ _ _ -> (world, Just startMessage)
    otherwise                         -> (world, Nothing)
    where
      startMessage  = Translate (1.5*x) 0  $ Color red $ Scale 0.2 0.2 $ Text "Press 's' for single player, 'm' for multiplayer"
      lossMessage s = Translate x ((fromIntegral mid) / 2) $ Color red $ Scale 0.3 0.3 $ Text s 
      x             = fromIntegral (-quarter)
      
-- sets a single bike one step forward in the current direction
move :: Bike -> Bike
move bike@(Bike _ dir (x,y) ps) =
  case dir of
    North -> bike {pos=(x, y+bikeWidth), prevPos=new_coords}
    South -> bike {pos=(x, y-bikeWidth), prevPos=new_coords} 
    East  -> bike {pos=(x+bikeWidth, y), prevPos=new_coords} 
    West  -> bike {pos=(x-bikeWidth, y), prevPos=new_coords}
    where
      new_coords = (x,y):ps
      
input :: Event -> World -> World
-- player 1 input
input 
  (EventKey (Char k) Down _ _) 
  w@(World _ bike2 _ _ started _ _ _) = do
    case k of
      'w' -> w' {p2bike= bike2 {dir=North}} 
      's' -> if started 
             then w' {p2bike= bike2 {dir=South}}
             else World player1 player2 False False True True newRands 0 -- set up new single player game      
      'a' -> w' {p2bike= bike2 {dir=West}} 
      'd' -> w' {p2bike= bike2 {dir=East}} 
      'm' -> if not started
             then World player1 player2 False False True False newRands 0   
             else w'    
      _   -> w' 
      where
        w' = w {aiRs = newRands}
        newRands = nextRands aiRands

  
-- player 2 inputs 
input 
  (EventKey (SpecialKey k) Down _ _ )
  w@(World bike1 _ _ _ _ False _ _) = do
    case k of
      KeyUp    -> w' {p1bike=bike1 {dir=North}} 
      KeyDown  -> w' {p1bike=bike1 {dir=South}} 
      KeyLeft  -> w' {p1bike=bike1 {dir=West}} 
      KeyRight -> w' {p1bike=bike1 {dir=East}} 
      _        -> w'  
      where
        w' = w {aiRs = newRands}
        newRands = nextRands aiRands
      
-- otherwise      
input e w = newRWorld w

-- main function to step through each frame
step :: Float -> World -> World
step _ w@(World _ _ _ _ True  single aiRands _) = 
  let wM@(World bike1 bike2 collided1 collided2 _ _ _ evaded) = (detectCollision . runAI) w 
      wAI@(World aibike _ _ _ _ _ newerRands _) = runAI w in 
  if collided1 || collided2
  then w {gameStarted=False, lastAIEvade=6}  -- stop the game, save state
  else if single
       then wAI {p1bike=move aibike, p2bike=move bike2, aiRs=nextRands newerRands} 
       else wM {p1bike=move bike1, p2bike=move bike2, aiRs=nextRands newerRands} 
                         
step _ w@(World _  _  _ _ False _ _ _) = w 

-- detect if either bike has collided in this frame
detectCollision :: World -> World
detectCollision w@(World bike1@(Bike _ _ pos1 coords1) bike2@(Bike _ _ pos2 coords2) _ _ _ _ _ _) =
  w {p1collision=hitAny pos1 pos2, p2collision=hitAny pos2 pos1} where
    hitAny p1 p2 = checkBounds p1 || (elem p1 (p2 : (coords1 ++ coords2)))
    -- check that bike is remaining on board
    checkBounds (x,y) = y >= mid || y <= -mid || x >= 2*quarter || x <= (-2)*quarter

-- Simple single player ai function

runAI :: World -> World
runAI w@(World bike1@(Bike bkcolor dir pos@(x,y) aiCoords) 
       bike2@(Bike _ _ playerPos playerCoords) False False started True (r:rs) lastEvade) = 
  if not $ null allMoves
  then w {p1bike=Bike bkcolor (fst $ head allMoves) pos aiCoords, aiRs=nextRands rs, lastAIEvade=evadeCount}
  else w {aiRs=nextRands rs} 
    where
      allMoves = gatherMoves bike1 bike2
      oldCoords = pos : playerPos : (aiCoords ++ playerCoords)
      (Bike _ _ nextMove _ )= move bike1
      willCrash (x', y') = 
        let (World _  _  aiCollide playerCollide _ _ _ _) = 
              detectCollision (World (Bike bkcolor dir (x', y') (pos:aiCoords)) (Bike red South playerPos playerCoords) 
                               False False True True rs evadeCount) in  (aiCollide, playerCollide)
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

runAI w = w        
