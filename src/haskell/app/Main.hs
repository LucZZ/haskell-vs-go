module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

rows :: Int
rows = 100
colums :: Int
colums = 100
padding :: Int
padding = 100
dimensions :: Int
dimensions = 10

windowWidth :: Int
windowWidth = padding  + rows * dimensions

windowHeight :: Int
windowHeight = padding  + colums * dimensions

window :: Display
window = InWindow "Conway's Game of Life" (windowWidth, windowHeight) (0, 0)

background :: Color
background = white

data GameState = Game
    {
        isRunning :: Bool,
        time :: Float,
        fps :: Int,
        aliveCells :: [(Float,Float)]
    }

initialGame :: GameState
initialGame = Game
    {
        isRunning = False,
        time = 0.0,
        fps = 20,
        aliveCells = [(1,1),(2,2),(0,3),(1,3),(2,3)]
    }

render :: GameState -> Picture
render state = pictures $ grid ++ aliveCellsGrid
    where aliveCellsGrid = [drawCell a b | (a, b) <- aliveCells state]

w = fromIntegral windowWidth - fromIntegral padding
h = fromIntegral windowHeight - fromIntegral padding

d = fromIntegral dimensions

grid :: [Picture]
grid = verticalLines ++ horizontalLines ++ [rectangleWire w h]
    where
    verticalLines = [vLine a | a <- [0 .. fromIntegral rows]]
    vLine a = color (greyN 0.5) $ line [(a * d - w / 2, -(h / 2)), (a * d - w / 2, h / 2)]
    horizontalLines = [hLine b | b <- [0 .. fromIntegral colums]]
    hLine b = color (greyN 0.5) $ line [(-(w / 2), b * d - h / 2), (w / 2, b * d - h / 2)]

drawCell :: Float -> Float -> Picture
drawCell x0 y0 = translate (x0 * d - w / 2 + d / 2) (-(y0 * d) + h / 2 - d / 2) square

square :: Picture
square = rectangleSolid d d

handleKeys (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) state =
    let
        gridX = (xPos + w / 2) / d
        gridY = -((yPos - h / 2) / d)
        roundedX = fromIntegral (floor gridX :: Int)
        roundedY = fromIntegral (floor gridY :: Int)
        newCell = (roundedX, roundedY)
    in
        if newCell `elem` aliveCells state
        then state
        else state { aliveCells = newCell : aliveCells state }

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) state = state {isRunning = not (isRunning state)}

handleKeys _ state = state 

update deltatime state 
    | isRunning state = state { aliveCells = updatedCells }
    | otherwise = state
  where
    updatedCells = map (\(x, y) -> (x + 1, y + 1)) (aliveCells state) --TODO

main :: IO ()
main = play 
            window
            background
            10
            initialGame
            render
            handleKeys
            update

