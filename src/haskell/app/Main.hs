module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

rows :: Int
rows = 50
colums :: Int
colums = 50
padding :: Int
padding = 100
dimensions :: Int
dimensions = 10

windowWidth :: Int
windowWidth = padding * 2 + rows * dimensions

windowHeight :: Int
windowHeight = padding * 2 + colums * dimensions

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

grid = verticalLines ++ horizontalLines ++ [rectangleWire w h]
    where verticalLines = foldr (\a -> \b -> vLine a:b) [] [0..fromIntegral colums] 
          vLine a = color  (greyN 0.5)  (line [ (w/fromIntegral colums*a-w/2, -h/2), (w/fromIntegral colums*a-w/2, h-h/2) ])
          horizontalLines = foldr (\a -> \b -> hLine a:b) [] [0..fromIntegral rows] 
          hLine a = color  (greyN 0.5)  (line [ (-w/2, h/fromIntegral rows*a-h/2), (w-w/2, h/fromIntegral rows*a-h/2) ])

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

