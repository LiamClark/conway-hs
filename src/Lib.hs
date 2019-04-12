module Lib where

import Data.Maybe        
import Control.Monad
import System.Random

data Cell = 
    Dead | Alive

-- assume the grid is square
newtype Grid = Grid [[Cell]]


createRandomGrid :: Int -> Int -> IO Grid
createRandomGrid x y = fmap Grid randomCells
    where 
        randomBools :: IO [[Bool]]
        randomBools = replicateM x (replicateM y randomIO)
        randomCells :: IO [[Cell]]
        randomCells = (fmap . fmap . fmap) (cellFromBool) randomBools

cellFromBool :: Bool -> Cell 
cellFromBool True = Alive
cellFromBool False = Dead

transition :: Grid -> Grid
transition (Grid grid) = Grid $ bimapIndexed grid (deadOrAlive (Grid grid))


deadOrAlive :: Grid -> Int -> Int -> Cell -> Cell
deadOrAlive grid x y cell = case cell of
                    Dead -> if aliveNeighbours == 3 then Alive else Dead
                    Alive -> case aliveNeighbours of 
                        0 -> Dead
                        1 -> Dead
                        2 -> Alive
                        3 -> Alive
                        _ -> Dead
                        
    where aliveNeighbours = length $ filter isAlive (neighbours grid x y)

isAlive :: Cell -> Bool
isAlive Dead = False
isAlive Alive = True
    
bimapIndexed :: [[a]] -> (Int -> Int -> a -> b) -> [[b]]
bimapIndexed xs f = mapIndexed xs (\xind ys -> mapIndexed ys (\yind cell -> f xind yind cell))

mapIndexed :: [a] -> (Int -> a -> b) -> [b]
mapIndexed xs f = fmap (uncurry f) indices
    where indices = zip [0..] xs

neighbours :: Grid -> Int -> Int -> [Cell]
neighbours grid x y = catMaybes $ uncurry (safeBiIndex grid) <$> possibleNeighbours
    where possibleNeighbours = neighbourIndices x y


safeBiIndex :: Grid -> Int -> Int -> Maybe Cell
safeBiIndex (Grid grid) x y = 
    if checkBounds grid x 
    then 
        let ys = grid !! x in
             if checkBounds ys y
             then Just $ ys !! y
             else Nothing
    else Nothing
    

neighbourIndices :: Int -> Int -> [(Int, Int)]
neighbourIndices x y = [
    (x + 1, y), (x - 1, y), -- left and right
    (x, y + 1), (x, y - 1), -- up and down
    (x + 1, y + 1), (x + 1, y - 1), -- diag right up and down.
    (x - 1, y + 1), (x - 1, y - 1) -- diag left up and down.
  ]

checkBounds :: [a] -> Int -> Bool
checkBounds xs ind = ind >= 0 && ind < length xs