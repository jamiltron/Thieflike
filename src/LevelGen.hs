module LevelGen where

import           Control.Monad (mapM)
import           Data.List     (zip4)
import qualified Data.Map      as M
import qualified Data.Set      as S
import           System.Random

import           Types

type GridID        = Coord
type Range         = (Coord, Coord)
type Connections   = M.Map GridID (S.Set GridID)

data Room = Room { rCorners :: Range
                 , rID      :: GridID
                 } deriving (Show)


go rows cols maxW maxH = do
  rooms <- genRooms rows cols maxW maxH
  conns <- genConnections rows cols
  putStrLn (digRooms rooms maxW maxH)
  return ()

-- Given a row and column length, generate
-- a random grid id within these bounds
randGridID :: Int -> Int -> IO GridID
randGridID row col = do
  x <- randomRIO(0, col - 1)
  y <- randomRIO(0, row - 1)
  return (x,y)


-- Insert into a connection map symmetrically, so that
-- (x,y) -> (w,z) also yields (w,z) -> (x,y)
symmInsert :: Connections -> Coord -> Coord -> Connections
symmInsert m (x,y) (w,z) =
  let m' = M.insertWith S.union (x,y) (S.singleton (w,z)) m
  in M.insertWith S.union (w,z) (S.singleton (x,y)) m'


-- Generate a map of connections based on our game's specifications
genConnections :: Int -> Int -> IO Connections
genConnections rows cols = genIter rows cols M.empty


-- Helper function to generate connections
genIter :: Int -> Int -> Connections -> IO Connections
genIter rows cols conns
  | rows * cols == M.size conns = return conns
  | otherwise = do
    (x,y) <- randGridID rows cols
    let adjs = adjRooms (x,y) rows cols
    room <- randomRIO(0, length adjs - 1)
    let toID   = adjs !! room
    let conns' = symmInsert conns (x,y) toID
    genIter rows cols conns'


-- Given a grid id and a max row and column length, generate
-- a list of all adjacent rooms to this room. In our game this
-- is all the cardinal directions from a given room.
adjRooms :: GridID -> Int -> Int -> [GridID]
adjRooms (x,y) rows cols = filter diffOne xys
  where xys = concat [[(x',y') | x' <- [x-1..x+1],
                                 x' >= 0 && x' < cols] |
                                 y' <- [y-1..y+1],
                                 y' >= 0 && y' < rows]
        diffOne (x',y') = abs(x' - x) + abs(y' - y) == 1


-- Randomly generate a list of ranges from min-point to
-- max-point for all the rooms on our map.
genRooms :: Int -> Int -> Int -> Int -> IO [Room]
genRooms rows cols maxX maxY = mapM (uncurry' genRoom) grids
  where
    xadj   = map (*roomH) [0..cols - 1]
    yadj   = map (*roomW) [0..rows - 1]
    roomW  = maxX `div` cols
    roomH  = maxY `div` rows
    area   = rows * cols
    adjs   = [(x,y) | x <- xadj, y <- yadj]
    ranges = [(c,r) | c <- [0..cols - 1], r <- [0..rows - 1]]
    grids  = zip4 adjs (replicate area roomW) (replicate area roomH) ranges
    uncurry' f ((a, b), c, d, e) = f (a, b) c d e


-- Randomly generate a min-point and max-point for a room with
-- a given min (x,y), as well as max width and heights and a
-- grid id
genRoom :: Coord -> Int -> Int -> GridID -> IO Room
genRoom c@(minX, minY) maxW maxH gID = do
  xstart <- randomRIO(minX   + 1, maxX - 6)
  ystart <- randomRIO(minY   + 1, maxY - 6)
  xend   <- randomRIO(xstart + 1, maxX - 1)
  yend   <- randomRIO(ystart + 1, maxY - 1)
  let dx  = xend - xstart
  let dy  = yend - ystart
  if dx < 6 || dy < 6 then genRoom c maxW maxH gID
    else return Room { rID = gID
                     , rCorners = ((xstart, ystart),
                                   (xend, yend))
                     }
  where
    maxX  = minX + maxW
    maxY  = minY + maxH


digRooms :: [Room] -> Int -> Int -> String
digRooms rooms maxW maxH = reverse (foldl (mapper dug) "" coords)
  where
    coords = concat [[(x,y) | x <- [0..maxW]] | y <- [0..maxH]]
    dug    = foldl M.union M.empty (map expandRange rooms)
    mapper tmap s (x,y)
      | x == maxW = '\n':s
      | otherwise = case M.lookup (x,y) tmap of
        Just c -> c:s
        _      -> ' ':s
      
-- expands a room into wall or floor symbols
expandRoom :: Room -> M.Map Coord Char
expandRoom (Room ((x0,y0), (x1,y1)) _) = foldl fill M.empty coords
  where
    coords = concat [[(x,y) | x <- [x0 - 1..x1 + 1]] |
                              y <- [y0 - 1..y1 + 1]]
    fill lmap (x,y)
      | x >= x0 &&
        x <= x1 &&
        y >= y0 &&
        y <= y1   = M.insert (x,y) '.' lmap
      | otherwise = M.insert (x,y) '#' lmap