module Graphics where

import System.Console.ANSI

import Level
import Types


coordToChar :: Coord -> World -> Char
coordToChar c (World depth hero lvl lvls)
  | hCurrPos hero == c  = '@'
  | isWall    c lvl     = '#'
  | isVillian c lvl     = 'v'
  | isPotion  c lvl     = '!'
  | isWeapon  c lvl     = ')'
  | isGold    c lvl     = '$'
  | isDStair  c lvl     = '<'
  | isUStair  c lvl     = '>'
  | otherwise           = ' '


drawChar :: Char -> IO ()
drawChar '@' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putChar '@'
drawChar '#' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putChar  '#'
drawChar '!' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Magenta]
  putChar '!'
drawChar '$' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Yellow ]
  putChar '$'
drawChar 'v' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red ]
  putStr "v"
drawChar ')' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Cyan ]
  putStr ")"
drawChar '>' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putStr ">"
drawChar '<' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putStr "<"
drawChar '\n' = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Vivid Black ]
  putStrLn ""
drawChar _ = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putStr " "


drawCoord :: World -> Coord -> IO ()
drawCoord world coord = do
  uncurry (flip setCursorPosition) coord
  drawChar (coordToChar coord world) 
  

drawHero :: World -> IO ()
drawHero world
  | newPos == oldPos = return ()
  | otherwise        = do
    drawCoord world newPos
    drawCoord world oldPos
  where
    hero   = wHero world
    newPos = hCurrPos hero
    oldPos = hOldPos  hero
  
drawWorld world = do
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
  where
    lvl = wLevel world
    chars = [[coordToChar (x,y) world | x <- [0..(fst (lMax lvl))]]
                                      | y <- [0..(snd (lMax lvl))]]
