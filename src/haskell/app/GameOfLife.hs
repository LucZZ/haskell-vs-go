module GameOfLife where
import Const

type Cell = (Int, Int)

gameStep :: [Cell] -> [Cell]
gameStep liveCells = [cell | cell <- allCells, shouldLive cell]
  where
    shouldLive cell
      | cell `elem` liveCells = liveNeighborCount cell `elem` [2, 3]
      | otherwise             = liveNeighborCount cell == 3
    liveNeighborCount (x, y) = length $ filter (`elem` liveCells) (neighbors x y)

allCells :: [Cell]
allCells = [(x, y) | x <- [0..rows-1], y <- [0..colums-1]]

neighbors :: Int -> Int -> [Cell]
neighbors x0 y0 = [(x, y) | x <- [(x0-1)..(x0+1)], y <- [(y0-1)..(y0+1)], (x, y) /= (x0, y0), cellInBounds x y]

cellInBounds :: Int -> Int -> Bool
cellInBounds x y = x >= 0 && y >= 0 && x < colums && y < rows