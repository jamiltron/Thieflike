module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

type Coord = (Int, Int)

data Direction = Up
               | Down
               | Left
               | Right

data Input = Dir Direction 
           | Exit

data World = World { wHero :: Coord }


-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


drawHero (heroX, heroY) = do
  clearScreen
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"


-- receive a character and return our Input data structure,
-- recursing on invalid input
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
dirToCoord Up    = (0, -1)
dirToCoord Down  = (0,  1)
dirToCoord Left  = (-1, 0)
dirToCoord Right = (1,  0)


-- add the supplied direction to the hero's position, 
-- and set that to be the hero's new position, making 
-- sure to limit it between 0 and 80 in either direction
handleDir w@(World hero) dir = gameLoop (w { wHero = newCoord })
  where newCoord       = (newX, newY)
        (heroX, heroY) = hero |+| dirToCoord dir
        hConst i       = max 0 (min i 80)
        newX           = hConst heroX
        newY           = hConst heroY


-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"
  

-- update the game loop to add in the goodbye message
gameLoop world@(World hero) = do
  drawHero hero
  input <- getInput
  case input of
    Exit    -> handleExit
    Dir dir -> handleDir world dir


main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  gameLoop $ World (0, 0)
