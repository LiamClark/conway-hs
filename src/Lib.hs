module Lib where

import Data.Maybe
import Control.Monad
import Data.Functor.Compose

type Cell = CellState
data CellState =
    Dead
  | Alive
  deriving (Eq, Show)

-- assume the grid is square
newtype Grid = Grid [[CellState]] deriving Show


makeGrid :: [[Int]] -> Grid
makeGrid myGrid = Grid
  [ [ cellFromBool element | element <- row ] | row <- myGrid ]

cellFromBool :: Int -> CellState
cellFromBool 0 = Dead
cellFromBool _ = Alive

neighbours  :: (Int, Int) -> [(Int,Int)]
neighbours (x,y) = [
    (x-1,y-1),
    (x-1,y  ),
    (x-1,y+1),
    (x+1,y-1),
    (x+1,y),
    (x+1,y+1),
    (x  ,y-1),
    (x  ,y+1)
  ]

getCellState :: Grid -> (Int, Int) -> CellState
getCellState grid@(Grid cells) (x,y)
  | isInBounds grid (x,y) = cells !! y !! x
  | otherwise = Dead

checkBounds :: [a] -> Int -> Bool
checkBounds xs y =
  y >= 0 && y < length xs

isInBounds :: Grid -> (Int, Int) -> Bool
isInBounds (Grid cells) (x,y) =
  checkBounds cells y && checkBounds (cells !! y) x

aliveNeighbours :: Grid -> (Int, Int) -> Int
aliveNeighbours grid position = aliveCells
  where
    aliveCells = length $ filter (== Alive) states
    states = map (getCellState grid) n
    n = neighbours position

updateCellState :: CellState -> Int -> CellState
updateCellState Alive 2 = Alive
updateCellState _ 3 = Alive
updateCellState _  _ = Dead

transition :: Grid -> Grid
transition grid@(Grid cells) = Grid $ mapWithIndices cells getNeighboursAndUpdate
    where
      getNeighboursAndUpdate y x cell =
        let
          alive = aliveNeighbours grid (x, y)
        in updateCellState cell alive

mapWithIndices :: [[a]] -> (Int -> Int -> a -> b) -> [[b]]
mapWithIndices xs f = mapWithIndex xs mapRow
  where mapRow cells yind = mapWithIndex cells mapElem
          where
            mapElem cell xind = f yind xind cell

mapWithIndex :: [a]-> (a -> Int -> b) -> [b]
mapWithIndex xs f = zipWith f xs [0..]


forallCells :: (Int -> Int -> Cell -> IO ()) -> Grid -> IO ()
forallCells action (Grid cells) = sequenceActions $ concat actions
    where
      actions :: [[IO ()]]
      actions  = mapWithIndices cells action


sequenceActions :: [IO ()] -> IO ()
sequenceActions (x : xs) = x >> sequenceActions xs
sequenceActions [] = return ()

sequence :: IO () -> IO () -> IO ()
sequence x y = x >> y

sequenceLists :: [[IO ()]] -> IO ()
sequenceLists xs = sequence_ $ Compose xs

mapIndexed :: [a] -> (Int -> a -> b) -> [b]
mapIndexed xs f = undefined


safeIndexGrid :: Grid -> Int -> Int -> Maybe Cell
safeIndexGrid (Grid grid) x y = undefined

neighbourIndices :: Int -> Int -> [(Int, Int)]
neighbourIndices = undefined

