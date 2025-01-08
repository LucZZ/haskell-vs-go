module Const where

columns :: Int
columns = 100
rows :: Int
rows = 100

padding :: Int
padding = 100

cellDimenstions :: Int
cellDimenstions = 10
cellDimensionsFloat :: Float
cellDimensionsFloat = fromIntegral cellDimenstions

gridHeight :: Float
gridHeight = fromIntegral rows * cellDimensionsFloat

gridWidth :: Float
gridWidth = fromIntegral columns * cellDimensionsFloat

windowHeight :: Int
windowHeight = rows * cellDimenstions + padding

windowWidth :: Int
windowWidth = columns * cellDimenstions + padding