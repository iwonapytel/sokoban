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

player2 :: Direction -> Picture
player2 L = playerL
player2 R = playerR
player _ = playerR

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
maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

tileToPic :: Integer -> Integer -> Picture
tileToPic x y = drawTile (maze (C x y))

-- Moving tiles
data Coord = C Integer Integer deriving Show
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

initialCoord :: Coord
initialCoord = C 0 0

data Direction = L | R | U | D

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord L (C x y) = (C (x-1) y)
adjacentCoord R (C x y) = (C (x+1) y)
adjacentCoord U (C x y) = (C x (y+1))
adjacentCoord D (C x y) = (C x (y-1))

moveCoords :: [Direction] -> Coord -> Coord
moveCoords [] coord = coord
moveCoords (h:t) coord = moveCoords t (adjacentCoord h coord)

-- Drawing maze
board :: [Picture]
board = [atCoord (C i j) (tileToPic i j) | i <- [-10..10], j <- [-10..10]]

pictureOfMaze :: Picture
pictureOfMaze = pictures(board)

initialPlayerCoord :: Coord
initialPlayerCoord = (C 0 2)

-- Moving player
movePlayer :: Direction -> Coord -> Coord
movePlayer dir c
     | comp (maze (adjacentCoord dir c)) Ground ||
       comp (maze (adjacentCoord dir c)) Storage = adjacentCoord dir c
     | otherwise = c

-- Interaction logic

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = movePlayer R c
    | key == "Up"    = movePlayer U c
    | key == "Left"  = movePlayer L c
    | key == "Down"  = movePlayer D c
handleEvent _ c      = c

drawState :: Coord -> Picture
drawState c = atCoord c playerL & pictureOfMaze

main :: IO ()
main = interactionOf initialPlayerCoord handleTime handleEvent drawState
