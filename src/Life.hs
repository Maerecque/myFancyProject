module Life
    ( ui
    ) where

import Data.List  (group,sort)      --for use of group and sort
import Control.Monad (guard,join)    -- for use of guard and join 
 
type AliveCell = (Int, Int)

type Grid = [AliveCell]

-- | function to see if the current living cell has a neighbour and return the copied location of him
neighbours :: AliveCell -> Grid
neighbours (x, y) = do
  xRange <- [-1..1]
  yRange <- [-1..1]
  guard (xRange /= 0 || yRange /= 0)
  return (x + xRange, y + yRange)

-- | function for making a new evolution
step :: Grid -> Grid
step aliveCells = do
  (newAliveCell, n) <- countingOccurences (join (neighbours <$> aliveCells))
  guard (n == 3 || n == 2 && newAliveCell `elem` aliveCells)
  return newAliveCell

-- | Couting Occurences of alive cells around eachother
countingOccurences :: Ord a => [a] -> [(a, Int)]
countingOccurences xs = do
  x <- group (sort xs)
  return (head x, length x)

-- | Function to format the entire grid and make sort of an UI
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

    gridSize = [(0,0),(50,20)] -- size of the grid
    maxX = gridRange fst
    maxY = gridRange snd
    gridRange f = [minGrid gridSize .. maxGrid gridSize] where
        minGrid = minimum . map f
        maxGrid = maximum . map f

-- | Function to print the entire grid
printGrid :: Grid -> IO ()
printGrid = putStrLn . formatGrid

-- | main function to set everything in motion
ui :: IO()
ui = mapM_ printGrid (take 31 (iterate step cellCoordinates)) where -- ammount of evolutions is declared here
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