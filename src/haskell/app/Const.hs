module Const where

rows :: Int
rows = 100
colums :: Int
colums = 100

padding :: Int
padding = 100

cellDimenstions :: Int
cellDimenstions = 10
cellDimensionsFloat :: Float
cellDimensionsFloat = fromIntegral cellDimenstions

gridHeight :: Float
gridHeight = fromIntegral colums * cellDimensionsFloat

gridWidth :: Float
gridWidth = fromIntegral rows * cellDimensionsFloat

windowHeight :: Int
windowHeight = colums * cellDimenstions + padding

windowWidth :: Int
windowWidth = rows * cellDimenstions + padding