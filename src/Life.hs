module Life
     where

import Data.List  (group,sort)      --for use of group and sort
import Control.Monad (guard,join)    -- for use of guard and join 

-- | AliveCell are the coordinates of an alive cell
type AliveCell = (Int, Int)

-- | Grid is the grid of all cells, alive or dead.
type Grid = [AliveCell]

neighbours :: AliveCell -> Grid
-- | Function to see if the current living cell has a neighbour and return the copied location of it.
neighbours (x, y) = do
  xRange <- [-1..1]
  yRange <- [-1..1]
  guard (xRange /= 0 || yRange /= 0)
  return (x + xRange, y + yRange)

step :: Grid -> Grid
-- | Function for making a new evolution.
step aliveCells = do
  (newAliveCell, n) <- countingOccurences (join (neighbours <$> aliveCells))
  guard (n == 3 || n == 2 && newAliveCell `elem` aliveCells)
  return newAliveCell

countingOccurences :: Ord a => [a] -> [(a, Int)]
-- | Couting Occurences of alive cells around the currently selected cell.
countingOccurences xs = do
  x <- group (sort xs)
  return (head x, length x)

formatGrid :: Grid -> String
-- | Function to format the entire grid and make sort of an UI.
formatGrid grid = do
  y <- maxY
  x <- maxX
  [marker x y] ++ endOfLine x where 
    -- Function to format a dead cell or an alive cell to a colored block on the screen:
    -- if there is an alive cell on the coordinate than mark it with a █, else put a blank space there
    marker :: Int -> Int -> Char
    marker x y      
      | (x, y) `elem` grid = ' '
      | otherwise          = '█'

    -- Function to print out the end of a line.
    endOfLine :: Int -> [Char] 
    endOfLine x      
      | x == maximum maxX = ['\n']
      | otherwise         = []

    gridSize = [(0,0),(50,20)] -- size of the grid
    maxX = gridRange fst
    maxY = gridRange snd
    gridRange f = [minGrid gridSize .. maxGrid gridSize] where
        minGrid = minimum . map f
        maxGrid = maximum . map f

printGrid :: Grid -> IO ()
-- | Function to print the entire grid
-- print the grid after being formatted
printGrid = putStrLn . formatGrid 

ui :: IO()
-- | Main function to set everything in motion
-- ammount of evolutions is declared here
ui = mapM_ printGrid (take 31 (iterate step cellCoordinates)) where 
    -- cellCoordinates = [(5, 0), (6, 1), (4, 2), (5, 2), (6, 2)] -- glider
    cellCoordinates = [ -- glider gun has takes 31 steps to be back in original state
      (2,6),(2,7),
      (3,6),(3,7),
      (12,6),(12,7),(12,8),
      (13,5),(13,9),
      (14,4),(14,10),
      (15,4),(15,10),
      (16,7),
      (17,5),(17,9),
      (18,6),(18,7),(18,8),
      (19,7),
      (22,4),(22,5),(22,6),
      (23,4),(23,5),(23,6),
      (24,3),(24,7),
      (26,2),(26,3),(26,7),(26,8),
      (36,4),(36,5),
      (37,4),(37,5)]