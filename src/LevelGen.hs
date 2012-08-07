module LevelGen where

import System.Random
import Data.List (zip4)
import Control.Monad (mapM)

import Types


type GridID = (Int, Int)

data Room = Room { rCoords    :: [Coord]
                 , rGrid      :: GridID
                 , rConnections :: [GridID]} deriving (Show)
            
-- 1. out of all the rooms, pick a random room
--    1a. Mark it as connected
-- 2. Select a random neighbor
-- 3. Connect the two rooms, set his room as current
-- 4. Repeat if any room is unconnected

genRooms :: Int -> Int -> Int -> Int -> IO [Room]
genRooms rows cols maxX maxY = mapM (uncurry' genRoom) grids
  where 
    xadj = map (*roomH) [0..cols]
    yadj = map (*roomW) [0..rows]
    rc = rows * cols
    adjs  = [(x,y) | x <- xadj, y <- yadj]
    pos   = [(c,r) | c <- [0..cols - 1], r <- [0..rows - 1]]
    roomW = maxX `div` cols
    roomH = maxY `div` rows
    grids = zip4 adjs (replicate rc roomW) (replicate rc roomH) pos
    uncurry' f ((a, b), c, d, e) = f a b c d e
        

genRoom :: Int -> Int -> Int -> Int -> GridID -> IO Room
genRoom minX minY maxW maxH gID = do
  xstart <- randomRIO(minX + 1,   maxX - 2)
  ystart <- randomRIO(minY + 1,   maxY - 2)
  xend   <- randomRIO(xstart + 1, maxX - 1)
  yend   <- randomRIO(ystart + 1, maxY - 1)
  return Room { rCoords = [(x,y) | x <- [xstart..xend], y <- [ystart..yend]],
                rConnections = [],
                rGrid = gID }
  where
    maxX = minX + maxW
    maxY = minY + maxH
  

