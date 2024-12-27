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
        fps :: Int
    }

initialGame :: GameState
initialGame = Game
    {
        isRunning = False,
        time = 0.0,
        fps = 20
    }

render state = pictures $ grid

w = fromIntegral windowWidth - fromIntegral padding 
h = fromIntegral windowHeight - fromIntegral padding 

d = fromIntegral dimensions

grid = verticalLines ++ horizontalLines ++ [rectangleWire w h]
    where
    verticalLines = [vLine a | a <- [0 .. fromIntegral rows]]
    vLine a = color (greyN 0.5) $ line [(a * d - w / 2, -(h / 2)), (a * d - w / 2, h / 2)]
    horizontalLines = [hLine b | b <- [0 .. fromIntegral colums]]
    hLine b = color (greyN 0.5) $ line [(-(w / 2), b * d - h / 2), (w / 2, b * d - h / 2)]


drawCell (x0, y0) = translate (x0 * d - w / 2 + d / 2) (-(y0 * d) + h / 2 - d / 2) square

square = rectangleSolid d d

handleKeys _ state = state

update deltatime state = state

main :: IO ()
main = play 
            window
            background
            60
            initialGame
            render
            handleKeys
            update

