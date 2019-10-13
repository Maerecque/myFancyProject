module Life
    ( someFunc
    ) where

import Data.List        --for use of group and sort
import Control.Monad    -- for use of guard

type AliveCell = (Int, Int)

type Grid = [AliveCell]

neighbours :: AliveCell -> Grid -- function to see if the current living cell has a neighbour and return the copied location of him
neighbours (x, y) = do
  xRange <- [-1..1]
  yRange <- [-1..1]
  guard (xRange /= 0 || yRange /= 0)
  return (x + xRange, y + yRange)

step :: Grid -> Grid
step aliveCells = do
  (newAliveCell, n) <- frequencies (join (neighbours <$> aliveCells))
  guard (n == 3 || n == 2 && newAliveCell `elem` aliveCells)
  return newAliveCell

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies xs = do
  x <- group (sort xs)
  return (head x, length x)

formatGrid :: Grid -> String
formatGrid grid = do
  y <- maxY
  x <- maxX
  [marker x y] ++ endOfLine x where 
    marker :: Int -> Int -> Char
    marker x y      -- if there is an alive cell on that coordinate than mark it with a *, else leave it empty
      | (x, y) `elem` grid = ' '
      | otherwise          = '█'
    endOfLine :: Int -> [Char]
    endOfLine x      -- wait untill you are the end of the grid then print a breakline
      | x == maximum maxX = ['\n']
      | otherwise         = []

    maxX = gridRange fst
    maxY = gridRange snd
    gridRange f = [minGrid grid .. maxGrid grid]
      where
        minGrid = minimum . map f
        maxGrid = maximum . map f

printGrid :: Grid -> IO ()
printGrid grid = do
  putStrLn (formatGrid grid)

someFunc :: IO()
someFunc = do
  mapM_ printGrid (take 30 (iterate step cellCoordinates))
  where
    -- beacon = [(0, 0), (1, 0), (0, 0), (3, 3), (2, 44)] --Locatie met sterren
    cellCoordinates = [(0,0),(1,0),(1,1),(0,1), (5, 0), (6, 1), (4, 2), (5, 2), (6, 2), (15, 15),(10,10),(11,10),(10,11),(11,11)] -- glider
    -- beacon = [(0, 0),(0,1),(1,0),(1,1),(2,0)]