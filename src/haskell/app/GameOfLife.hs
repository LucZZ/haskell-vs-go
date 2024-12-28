module GameOfLife where
import Const

gameStep :: [(Int, Int)] -> [(Int, Int)]
gameStep = map (\(x, y) -> (x + 1, y + 1))

allCells :: [(Int, Int)]
allCells = [(x, y) | x <- [0..rows-1], y <- [0..colums-1]]