{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as Text

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
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

-- Maze
data Level = Lvl Coord Maze
type Maze = Coord -> Tile
defaultLevel :: Level
defaultLevel = Lvl start maze
  where
    start = (C (-3) 3)
    maze (C x y)
      | abs x > 4  || abs y > 4  = Blank
      | abs x == 4 || abs y == 4 = Wall
      | x ==  2 && y <= 0        = Wall
      | x ==  3 && y <= 0        = Storage
      | x >= -2 && y == 0        = Box
      | otherwise                = Ground

level2 :: Level
level2 = Lvl start maze
  where
    start = (C 0 0)
    maze (C x y)
      | abs x > 4 || abs y > 4               = Blank
      | abs x == 4 || abs y == 4             = Wall
      | abs y == 2 && abs x > 1              = Wall
      | y == 0 && (abs x == 2 || abs x == 1) = Box
      | abs y == 3 && abs x == 3             = Storage
      | otherwise                            = Ground

level3 :: Level
level3 = Lvl start maze
  where
    start = (C 0 0)
    maze (C x y)
      | abs x > 4 || y > 4 || y < (-2)    = Blank
      | y == 4 || y == (-2) || abs x == 4 = Wall
      | y == (-1) && x /= 0 && x /= (-1)  = Wall
      | y == 1 && x > -1 && x < 3         = Wall
      | y == 3 && x > -1                  = Wall
      | y == 2 && x > 0 && x < 4          = Storage
      | y == 2 && x == 0                  = Box
      | y == 0 && (x == -1 || x == -2)    = Box
      | otherwise                         = Ground


unpackMaze :: Level -> Maze
unpackMaze (Lvl start maze) = maze

unpackStart ::Level -> Coord
unpackStart (Lvl start maze) = start

-- Moving tiles
data Coord = C Integer Integer deriving (Show, Eq)
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

data Direction = L | R | U | D deriving Eq

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord L (C x y) = (C (x-1) y)
adjacentCoord R (C x y) = (C (x+1) y)
adjacentCoord U (C x y) = (C x (y+1))
adjacentCoord D (C x y) = (C x (y-1))

-- Drawing maze
board :: Maze -> [Picture]
board maze = [atCoord coord (drawTile (maze coord)) | i <- [-10..10], j <- [-10..10], let coord = (C i j)]

pictureOfMaze :: Maze -> Picture
pictureOfMaze m = pictures(board m)

-- Changing state
isOkToMove :: State -> Bool
isOkToMove state@(S c dir boxes lvl) =
  (mazeWithoutBoxes c == Ground || mazeWithoutBoxes c == Storage) &&
  (not (elem c boxes))
  where
    mazeWithoutBoxes = removeBoxes (getNthMaze lvl)

moveBox :: Direction -> State -> State
moveBox dir state@(S c d boxes lvl) =
    (S c d (map func boxes) lvl)
  where
    func = (\x -> if (x == c) then (adjacentCoord dir x) else x)

adjacentState :: Direction -> State -> State
adjacentState dir state@(S c d boxes lvl) =
  (S (adjacentCoord dir c) dir boxes lvl)

changeState :: Direction -> State -> State
changeState dir state@(S coord curr_dir boxes lvl) =
  if (isOkToMove adjState) then adjState
  else if (elem adjCoord boxes && isOkToMove (adjacentState dir adjState)) then
    moveBox dir adjState
  else
    state
  where
    adjCoord = adjacentCoord dir coord
    adjState = adjacentState dir state

changeLevel :: State -> State
changeLevel (S c dir boxes lvl) = initialLevelState (lvl + 1)

-- Interactions (polymorphism)
data Interaction world = Interaction
        world
	(Double -> world -> world)
	(Event -> world -> world)
	(world -> Picture)

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")
data SSState world = StartScreen | Running world | Winning world deriving Eq

resettable :: Interaction s -> Interaction s
resettable (Interaction state0 step handle draw)
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

textToDir :: Text.Text -> Maybe Direction
textToDir "Right" = Just R
textToDir "Up"    = Just U
textToDir "Left"  = Just L
textToDir "Down"  = Just D
textToDir _       = Nothing

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
  | isWinning state = changeLevel state
handleEvent (KeyPress key) state =
  case newDir of
    Just dir -> changeState dir state
    Nothing -> state
  where
    newDir = textToDir key
handleEvent _ state = state

-- Drawing states

data State = S Coord Direction [Coord] Int deriving Eq

drawState :: State -> Picture
drawState state
  | isWinning state = scaled 3 3 (text "Win!") & translated 0 (-2) (scaled 0.7 0.7 (text "(Press any key)"))
drawState (S c dir boxes lvl) = atCoord c player & pictureWithBoxes
  where
    player = if (dir == L) then playerL else playerR
    mazeWithoutBoxes = removeBoxes (getNthMaze lvl)
    pictureWithBoxes = pictureOfMaze (addBoxes boxes mazeWithoutBoxes)

-- Initial values

initialLevel :: Int
initialLevel = 1

initialLevelState :: Int -> State
initialLevelState lvl =
  (S coord direction boxes level)
  where
    level = case (nth lvl okLevels) of
      Just l -> lvl
      Nothing    -> initialLevel
    coord = getNthStart level
    direction = R
    boxes = getInitialBoxes (getNthMaze level)

initialState :: State
initialState = initialLevelState initialLevel

-- Boxes helper functions

drawingBoxes :: Maze -> [Coord] -> [Picture]
drawingBoxes maze list = [ atCoord coord (drawTile (maze coord)) | coord <- list]

removeBoxes :: Maze -> Maze
removeBoxes maze = f . maze
  where
    f = (\x -> if (x == Box) then Ground else x)

addBoxes :: [Coord] -> Maze -> Maze
addBoxes coords maze = (\x -> if (elem x coords) then Box else (maze x))

getInitialBoxes :: Maze -> [Coord]
getInitialBoxes maze =  [ coord | i <- [-10..10], j <- [-10..10],
                         let coord = (C i j), (maze coord) == Box]

-- Winning

isWinning :: State -> Bool
isWinning (S c dir boxes lvl) = allList (map isOnStorage boxes)
  where
    mazeWithoutBoxes = removeBoxes (getNthMaze lvl)
    isOnStorage = (\x -> (mazeWithoutBoxes x) == Storage)

-- withUndo

data WithUndo a = WithUndo a [a]

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 step handle draw) = Interaction state0' step' handle' draw' where
    state0' = WithUndo state0 []
    step' t (WithUndo s stack) = WithUndo (step t s) stack
    handle' (KeyPress key) (WithUndo s stack)
      | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where
        s' = handle e s
    draw' (WithUndo s _) = draw s

-- List functions

allList :: [Bool] -> Bool
allList bools = not (elem False bools)

nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 1 (x : _)  = Just x
nth n (x : xs) = nth (n - 1) xs

-- Graph functions

doDfs :: Eq a => [a] -> a -> (a -> [a]) -> [a]
doDfs visited u neighbours
  | elem u visited = []
  | otherwise = [u] ++ (concat (map (\x -> doDfs (visited ++ [u]) x neighbours) (neighbours u)))

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk =
  allList vertices
  where
    vertices = map isOk (doDfs [] initial neighbours)

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours =
  elem v visited
  where
    visited = doDfs [] initial neighbours

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours =
  allList (map (\x -> reachable x initial neighbours) vs)

-- Checking levels
adjacentVertices :: Coord -> [Coord]
adjacentVertices (C x y) = [(C (x-1) y), (C x (y-1)), (C (x+1) y), (C x (y+1))]

isClosed :: Level -> Bool
isClosed level = isGraphClosed initial neigbours isOk
  where
    initial = unpackStart level
    maze = unpackMaze level
    neigbours :: Coord -> [Coord]
    neigbours v = filter (\x -> (maze x) /= Wall)(adjacentVertices v)
    isOk :: Coord -> Bool
    isOk v = (maze v) /= Blank

isSane :: Level -> Bool
isSane level = length storages >= length boxes
  where
    initial = unpackStart level
    maze = unpackMaze level
    isOk v = (maze v) /= Blank && (maze v) /= Wall
    neighbours :: Coord -> [Coord]
    neighbours v = filter isOk (adjacentVertices v)
    storages = filter (\x -> (maze x) == Storage) (doDfs [] initial neighbours)
    boxes = filter (\x -> (maze x) == Box) (doDfs [] initial neighbours)

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

-- The above part doesn't really work - because of DFS probably

-- main :: IO()
-- main = drawingOf(pictureOfBools (map isClosed okLevels))

-- Lists of levels

okLevels :: [Level]
okLevels = [level3, level2, defaultLevel]

badLevels :: [Level]
badLevels = []

getNthMaze :: Int -> Maze
getNthMaze i =
  case (nth i okLevels) of
    Just (Lvl start maze) -> maze
    Nothing               -> defaultMaze
  where
    defaultMaze = unpackMaze defaultLevel

getNthStart :: Int -> Coord
getNthStart i =
  case (nth i okLevels) of
    Just (Lvl start maze) -> start
    Nothing               -> defaultStart
  where
    defaultStart = unpackStart defaultLevel


-- Walks
walk5 :: IO ()
walk5 = runInteraction (withUndo (withStartScreen( resettable (Interaction initialState handleTime handleEvent drawState))))

-- Main
main :: IO ()
main = walk5
