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

drawGridSimple :: Grid -> IO ()
drawGridSimple [] = return ()
drawGridSimple (row:rows) = do
    putStrLn (map drawCell row) 
    drawGrid rows               
  where
    drawCell True  = 'X'
    drawCell False = '.'

main :: IO ()
main = drawGridSimple initGrid
