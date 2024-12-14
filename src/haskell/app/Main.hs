module Main where

type Cell = Bool
type Grid = [[Cell]]

initGrid :: Grid
initGrid = 
      [ [False, False, False, False, False],
        [False, False, False, False, False],
        [False, False, True, False, False],
        [False, False, False, False, False],
        [False, False, False, False, False] ]

drawGrid :: Grid -> IO ()
drawGrid grid = mapM_ (putStrLn . map drawCell) grid
  where
    drawCell True  = 'X' 
    drawCell False = '.'

-- Simple method with recursion
drawGridSimple :: Grid -> IO ()
drawGridSimple [] = return ()
drawGridSimple (row:rows) = do
    putStrLn (map drawCellSimple row) 
    drawGridSimple rows               

drawCellSimple :: Bool -> Char
drawCellSimple True = 'X'
drawCellSimple False = '.'

cellInGrid :: Grid -> Int -> Int -> Bool
cellInGrid grid x y = x >= 0 && y >= 0 && x < length grid && y < length (head grid)

countAliveNeighbours :: Grid -> Int -> Int -> Int
countAliveNeighbours grid x y = length  [() | i <- [x-1..x+1], j <- [y-1..y-1], i /= x || j /= y, cellInGrid grid i j, grid !! i !! j]

nextStateForCell :: Grid -> Int -> Int -> Cell
nextStateForCell grid x y = 
    let neighbours = countAliveNeighbours grid x y
        cell = grid !! x !! y
    in neighbours == 3 || (cell && neighbours == 2)

nextGeneration :: Grid -> Grid
nextGeneration grid = [[nextStateForCell grid x y | y <- [0..length (head grid) - 1]]
                                                  | x <- [0..length grid - 1]]

main :: IO ()
main = drawGridSimple initGrid
