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

genesis = World (0,0)


drawWorld :: World -> IO ()
drawWorld w = do
  clearScreen
  drawHero $ wHero w


drawHero :: Coord -> IO ()
drawHero (heroX, heroY) = do
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"


getInput :: IO Input
getInput = do
  c <- getChar
  case c of
    '\ESC' -> return Esc
    'w'    -> return Up
    's'    -> return Down
    'a'    -> return Left
    'd'    -> return Right
    _      -> getInput


gameLoop :: World -> IO ()
gameLoop w = do
  drawWorld w
  input <- getInput
  case input of 
    Esc -> handleExit
    _   -> handleDir w input
    

handleExit = do
  clearScreen
  setCursorPosition 0 0
  showCursor
  putStrLn "Thank you for playing!"


handleDir w@(World h) i = gameLoop (w { wHero = h |+| i })


main :: IO ()
main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Thieflike"
  clearScreen
  gameLoop genesis