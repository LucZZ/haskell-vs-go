module Const where

rows :: Int
rows = 100
columns :: Int
columns = 100

padding :: Int
padding = 100

cellDimenstions :: Int
cellDimenstions = 10
cellDimensionsFloat :: Float
cellDimensionsFloat = fromIntegral cellDimenstions

gridHeight :: Float
gridHeight = fromIntegral columns * cellDimensionsFloat

gridWidth :: Float
gridWidth = fromIntegral rows * cellDimensionsFloat

windowHeight :: Int
windowHeight = columns * cellDimenstions + padding

windowWidth :: Int
windowWidth = rows * cellDimenstions + padding