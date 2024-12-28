module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Const
import GameOfLife

window :: Display
window = InWindow "Conway's Game of Life" (windowWidth, windowHeight) (0, 0)

background :: Color
background = white

data GameState = Game
    {
        isRunning :: Bool,
        aliveCells :: [(Int,Int)]
    }

initialGame :: GameState
initialGame = Game
    {
        isRunning = False,
        aliveCells = [(1,1),(2,2),(0,3),(1,3),(2,3)]
    }

render :: GameState -> Picture
render state = pictures $ grid ++ aliveCellsGrid
    where aliveCellsGrid = [drawCell a b | (a, b) <- aliveCells state]

grid :: [Picture]
grid = verticalLines ++ horizontalLines ++ [rectangleWire gridWidth gridHeight]
    where
    verticalLines = [vLine a | a <- [0 .. fromIntegral rows]]
    vLine a = color (greyN 0.5) $ line [(a * cellDimensionsFloat - gridWidth / 2, -(gridHeight / 2)), (a * cellDimensionsFloat - gridWidth / 2, gridHeight / 2)]
    horizontalLines = [hLine b | b <- [0 .. fromIntegral colums]]
    hLine b = color (greyN 0.5) $ line [(-(gridWidth / 2), b * cellDimensionsFloat - gridHeight / 2), (gridWidth / 2, b * cellDimensionsFloat - gridHeight / 2)]

drawCell :: Int -> Int -> Picture
drawCell x0 y0 = translate (fromIntegral x0 * cellDimensionsFloat - gridWidth / 2 + cellDimensionsFloat / 2) (-(fromIntegral y0 * cellDimensionsFloat) + gridHeight / 2 - cellDimensionsFloat / 2) square

square :: Picture
square = rectangleSolid cellDimensionsFloat cellDimensionsFloat

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) state =
    let
        gridX = (xPos + gridWidth / 2) / cellDimensionsFloat
        gridY = -((yPos - gridHeight / 2) / cellDimensionsFloat)
        roundedX = fromIntegral (floor gridX :: Int)
        roundedY = fromIntegral (floor gridY :: Int)
        newCell = (roundedX, roundedY)
    in
        if newCell `elem` aliveCells state
        then state
        else state { aliveCells = newCell : aliveCells state }

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) state = state {isRunning = not (isRunning state)}

handleKeys _ state = state 

update :: p -> GameState -> GameState
update deltatime state 
    | isRunning state = state { aliveCells = updatedCells }
    | otherwise = state
  where
    updatedCells = gameStep (aliveCells state) --TODO

main :: IO ()
main = play 
            window
            background
            10
            initialGame
            render
            handleKeys
            update

