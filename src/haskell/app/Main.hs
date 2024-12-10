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
countAliveNeighbours grid x y = length  [() | i <- [x-1..x+1], j <- [y-1..y-1], (i /= x || j /= y), cellInGrid grid i j, grid !! i !! j]

main :: IO ()
main = drawGridSimple initGrid
