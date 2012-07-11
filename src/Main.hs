--file: Main.hs
module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

type Coord = (Int, Int)

data World = World { wHero :: Coord }

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)

main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  gameLoop $ World (0, 0)


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
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _ -> getInput


-- operator to add 2 coordinates together
(|+|) :: Coord -> Coord -> Coord
(|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


dirToCoord d
  | d == Up    = (0, -1)
  | d == Down  = (0,  1)
  | d == Left  = (-1, 0)
  | d == Right = (1,  0)
  | otherwise  = (0,  0)
                 

-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleDir w@(World hero) input = gameLoop (w { wHero = newCoord })
  where newCoord       = (newX, newY)
        (heroX, heroY) = hero |+| (dirToCoord input)
        hConst i       = max 0 (min i 80)
        newX           = hConst heroX
        newY           = hConst heroY
        

-- update the game loop to add in the goodbye message
gameLoop world@(World hero) = do
  drawHero hero
  input <- getInput
  case input of
    Exit -> handleExit
    _    -> handleDir world input


-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"
