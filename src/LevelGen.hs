module LevelGen where

import           Control.Monad         (mapM, liftM)
import           Data.Lens.Common      ((^=))
import           Data.List             (zip4)
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S
import           System.Random
import           System.Random.Shuffle (shuffle')

import           Types


(|*|) (a,b) (c,d) = (a*c, b*d)

minHeight = 6
minWidth  = 6

-- given a set of rows, cols, maxW, maxH, generate
-- a string representing a new map
genLevel :: Int -> Int -> Int -> Int -> IO [String]
genLevel rows cols maxWidth maxHeight = do
  rooms <- mapM (genRoom rows cols colWidth rowHeight) grid
  let level = digRooms rooms maxWidth maxHeight
  return $ lines level
  where
    grid      = concat [[(c,r) | c <- [0..cols - 1]] | r <- [0..rows - 1]]
    colWidth  = maxWidth  `div` cols
    rowHeight = maxHeight `div` rows


genRoom :: Int -> Int -> Int -> Int -> GridID -> IO Room
genRoom rows cols colWidth rowHeight (c,r) = do
  xstart <- randomRIO(minX   + 1, minX + (colWidth  - 7))
  ystart <- randomRIO(minY   + 1, minY + (rowHeight - 7))
  xend   <- randomRIO(xstart + minWidth, minX + (colWidth   - 1))
  yend   <- randomRIO(ystart + minHeight, minY + (rowHeight - 1))
  genConns rows cols (Room S.empty ((xstart,ystart),(xend,yend)) (c,r))
  where
    (minX, minY) = (c * colWidth, r * rowHeight)

    
genConns :: Int -> Int -> Room -> IO Room
genConns rows cols room@(Room _ _ (c,r)) = do
  numConns    <- randomRIO (1, length adjs)
  roomShuffle <- shuffleList adjs
  return (room { rConns = S.fromList (take numConns roomShuffle) } )
  where
    adjs = adjRooms (c,r) rows cols


shuffleList :: [a] -> IO [a]
shuffleList l = do
  seed <- newStdGen
  return (shuffle' l (length l) seed)
  

-- Given a row and column length, generate
-- a random grid id within these bounds
randGridID :: Int -> Int -> IO GridID
randGridID row col = do
  x <- randomRIO(0, col - 1)
  y <- randomRIO(0, row - 1)
  return (x,y)


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

-- merge all of the symbols for all the rooms into a string
digRooms :: [Room] -> Int -> Int -> String
digRooms rooms maxW maxH = reverse $ foldl (mapper dug) "" coords
  where
    coords = concat [[(x,y) | x <- [0..maxW]] | y <- [0..maxH]]
    dug    = foldl M.union M.empty (map expandRoom rooms)
    mapper tmap s (x,y)
      | x == maxW = '\n':s
      | otherwise = case M.lookup (x,y) tmap of
        Just c -> c:s
        _      -> ' ':s


-- expands a room into wall or floor symbols
expandRoom :: Room -> CharMap
expandRoom (Room _ ((x0,y0), (x1,y1)) _) = foldl fill M.empty coords
  where
    coords = concat [[(x,y) | x <- [x0 - 1..x1 + 1]] |
                              y <- [y0 - 1..y1 + 1]]
    fill lmap (x,y)
      | x >= x0 &&
        x <= x1 &&
        y >= y0 &&
        y <= y1   = M.insert (x,y) '.' lmap
      | otherwise = M.insert (x,y) '#' lmap

connectCoords :: Coord -> Coord -> CharMap -> CharMap
connectCoords (x,y) (w,z) cmap
  | (x,y) == (w,z) = cmap
  | dx > dy   = if x < w
                then connectCoords (x + 1, y) (w, z) (hallInsert (x,y) cmap)
                else connectCoords (x - 1, y) (w, z) (hallInsert (x,y) cmap)
  | otherwise = if y < z
                then connectCoords (x, y + 1) (w, z) (hallInsert (x,y) cmap)
                else connectCoords (x, y - 1) (w, z) (hallInsert (x,y) cmap)
  where dx = abs (x - w)
        dy = abs (y - z)


hallInsert :: Coord -> CharMap -> CharMap
hallInsert (x,y) m = let m' = M.insert (x,y) '.' m
                     in foldl fill m' [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
  where
    fill lmap (x,y) = M.insertWith (\a b -> if b == ' ' then a else b) (x,y) '#' lmap


