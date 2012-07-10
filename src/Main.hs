module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO

type Coord = (Int, Int)

(|+|) :: Coord -> Input -> Coord
(|+|) (x,y) d
  | d == Up    = (x, y - 1)
  | d == Down  = (x, y + 1)
  | d == Left  = (x - 1, y)
  | d == Right = (x + 1, y)
  | otherwise  = (x, y)

data Input = Up
           | Down
           | Left
           | Right
           | Esc
           deriving (Eq)

data World = World { wHero  :: Coord }

-- a starting world
genesis = World (0,0)


-- We take a world, clear the screen and then draw all
-- of the world's entities, which right now is just the hero
drawWorld :: World -> IO ()
drawWorld world = do
  clearScreen
  drawHero $ wHero world


drawHero :: Coord -> IO ()
drawHero (heroX, heroY) = do
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"


-- recieve a character and return our Input data structure,
-- recursing on invalid input
getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    '\ESC' -> return Esc
    'w'    -> return Up
    's'    -> return Down
    'a'    -> return Left
    'd'    -> return Right
    _      -> getInput


-- draw the world, grab input and then handle the input
gameLoop :: World -> IO ()
gameLoop world = do
  drawWorld world
  input <- getInput
  case input of 
    Esc -> handleExit
    _   -> handleDir world input
    

-- when the user wants to exit we give them a thank you
-- message and then reshow the cursor
handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"


-- add the supplied direction to the hero's position, and set that
-- to be the hero's new position, making sure to limit the hero's
-- position between 0 and 80 in either direction
handleDir w@(World hero) input = gameLoop (w { wHero = newCoord })
  where newCoord         = (newX, newY)
        (heroX, heroY) = hero |+| input
        newX             = max 0 (min heroX 80)
        newY             = max 0 (min heroY 80)


main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  clearScreen
  gameLoop genesis