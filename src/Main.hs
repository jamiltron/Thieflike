module Main where

import Data.Lens.Common ((^.), (^=))
import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

import Console
import Level
import Types


-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return Exit
    'w' -> return (Dir Up)
    's' -> return (Dir Down)
    'a' -> return (Dir Left)
    'd' -> return (Dir Right)
    _   -> getInput


-- translate a direction to a coordinate so it can be added to
-- the hero's coordinate to move the hero around
dirToCoord :: Direction -> Coord
dirToCoord Up    = (0, -1)
dirToCoord Down  = (0,  1)
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)


-- add the supplied direction to the hero's position, 
-- and set that to be the hero's new position, making 
-- sure to limit it between 0 and 80 in either direction
handleDir :: World -> Direction -> IO ()
handleDir w dir
  | isWall coord lvl ||
    isClosedDoor coord lvl = gameLoop ((^=) posL (w ^. posL) w)
 
  | otherwise              = gameLoop ((^=) posL coord w)
  where 
    h              = wHero w
    lvl            = wLevel w
    coord          = (newX, newY)
    newX           = hConst heroX
    newY           = hConst heroY
    (heroX, heroY) = hCurrPos h |+| dirToCoord dir
    hConst i       = max 0 (min i 80)


-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit :: IO ()
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  setSGR [Reset]
  putStrLn "Thank you for playing!"


-- draw the hero, process input, and either recur or exit
gameLoop :: World -> IO ()
gameLoop world = do
  drawHero world
  input <- getInput
  case input of
    Exit    -> handleExit
    Dir dir -> handleDir world dir

main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  clearScreen
  let world = genesis { wLevel = level1, wLevels = [level1] }
  drawWorld world
  gameLoop world
