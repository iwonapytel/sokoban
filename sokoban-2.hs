{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Tiles definitions
wall, ground, storage, box, cross, field :: Picture
field = solidRectangle 1 1
cross = path[(0.5, 0.5), (-0.5, -0.5)] & path[(0.5, -0.5), (-0.5, 0.5)]
wall = colored (dull red) field
ground = colored brown field
storage = colored blue (solidCircle 0.2) & ground
box = colored blue cross & colored azure field

-- Player definition
playerL, playerR :: Picture
playerR = translated 0.2 0.0 (colored white (solidCircle 0.15)) & solidCircle 0.4
playerL = translated (-0.2) 0.0 (colored white (solidCircle 0.15)) & solidCircle 0.4

-- Drawing tiles
data Tile = Wall | Ground | Storage | Box | Blank
drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

comp :: Tile -> Tile -> Bool
comp Wall Wall = True
comp Ground Ground = True
comp Storage Storage = True
comp Box Box = True
comp Blank Blank = True
comp _ _ = False

-- Maze
type Maze = Coord -> Tile
maze :: Maze
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

tileToPic :: Maze -> Integer -> Integer -> Picture
tileToPic m x y = drawTile (m (C x y))

-- Moving tiles
data Coord = C Integer Integer deriving Show
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

-- Coord getters
toFirstCoord :: Coord -> Integer
toSecondCoord :: Coord -> Integer
toFirstCoord (C x y) = x
toSecondCoord (C x y) = y

instance Eq Coord where
  a == b = (toFirstCoord a == toFirstCoord b) && (toSecondCoord a == toSecondCoord b)

data Direction = L | R | U | D

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord L (C x y) = (C (x-1) y)
adjacentCoord R (C x y) = (C (x+1) y)
adjacentCoord U (C x y) = (C x (y+1))
adjacentCoord D (C x y) = (C x (y-1))

-- moveCoords :: [Direction] -> Coord -> Coord
-- moveCoords [] coord = coord
-- moveCoords (h:t) coord = moveCoords t (adjacentCoord h coord)

-- Drawing maze
board :: Maze -> [Picture]
board m = [atCoord (C i j) (tileToPic m i j) | i <- [-10..10], j <- [-10..10]]

pictureOfMaze :: Maze -> Picture
pictureOfMaze m = pictures(board m)

-- Changing state

isBoxMovable :: Direction -> State -> Bool
isBoxMovable dir (S c curr_dir boxes)
    | (comp (mazeWithoutBoxes (adjacentCoord dir c)) Ground ||
      comp (mazeWithoutBoxes (adjacentCoord dir c)) Storage) &&
      not (elem (adjacentCoord dir c) boxes) = True
    | otherwise = False

changeState :: Direction -> State -> State
changeState dir (S coord curr_dir boxes)
    | (elem (adjacentCoord dir coord) boxes) && (isBoxMovable dir (S (adjacentCoord dir coord) dir boxes)) =
      (S (adjacentCoord dir coord) dir (map (\x -> if x == (adjacentCoord dir coord) then (adjacentCoord dir x) else x) boxes))
    | not (elem (adjacentCoord dir coord) boxes) &&
      (comp (mazeWithoutBoxes (adjacentCoord dir coord)) Ground ||
      comp (mazeWithoutBoxes (adjacentCoord dir coord)) Storage) = (S (adjacentCoord dir coord) dir boxes)
    | otherwise = (S coord curr_dir boxes)

-- Interactions (polymorphism)
data Interaction world = Interaction
        world
	(Double -> world -> world)
	(Event -> world -> world)
	(world -> Picture)

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")
data SSState world = StartScreen | Running world

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state step handle draw) = interactionOf state step handle draw

-- Handlers

handleTime :: Double -> State -> State
handleTime _ s = s

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
    | key == "Right" = changeState R state
    | key == "Up"    = changeState U state
    | key == "Left"  = changeState L state
    | key == "Down"  = changeState D state
handleEvent _ state = state

-- Drawing states

data State = S Coord Direction [Coord]

drawState :: State -> Picture
drawState state
  | isWinning state = scaled 3 3 (text "Win!") & translated 0 (-2) (scaled 0.7 0.7 (text "(Press escape to restart)"))
drawState (S c L boxes) = atCoord c playerL & pictureOfMaze (addBoxes boxes mazeWithoutBoxes)
drawState (S c R boxes) = atCoord c playerR & pictureOfMaze (addBoxes boxes mazeWithoutBoxes)
drawState (S c dir boxes) = atCoord c playerR & pictureOfMaze (addBoxes boxes mazeWithoutBoxes)

-- Initial values

mazeWithoutBoxes :: Maze
mazeWithoutBoxes = removeBoxes maze

initialCoord :: Coord
initialCoord = C 0 2

initialDirection :: Direction
initialDirection = R

initialBoxes :: [Coord]
initialBoxes = [ (C i j) | i <- [-10..10], j <- [-10..10], comp (maze (C i j)) Box == True]

initialState = (S initialCoord initialDirection initialBoxes)

-- Boxes helper functions

drawingBoxes :: Maze -> [Coord] -> [Picture]
drawingBoxes maze list = [ atCoord (C x y) (tileToPic maze x y) | (C x y) <- list]

removeBoxes :: Maze -> Maze
removeBoxes maze = f . maze
  where
    f = (\x -> if (comp x Box) == True then Ground else x)

addBoxes :: [Coord] -> Maze -> Maze
addBoxes coords maze = (\x -> if (elem x coords) then Box else (maze x))

-- Winning

allList :: [Bool] -> Bool
allList bools = not (elem False bools)

isWinning :: State -> Bool
isWinning (S c dir boxes) = allList (map isOnStorage boxes)
  where
    isOnStorage = (\x -> comp (mazeWithoutBoxes x) Storage)

-- Main
main :: IO ()
main = runInteraction (withStartScreen (resetable (Interaction initialState handleTime handleEvent drawState)))
