module Console where

import System.Console.ANSI

import Level
import Types


coordToChar :: Coord -> World -> Char
coordToChar coord (World _ hero lvl _)
  | hCurrPos hero == coord      = '@'
  | isAcid        coord lvl     = '~'
  | isClosedDoor  coord lvl     = '+'
  | isOpenDoor    coord lvl     = '-'
  | isDownstairs  coord lvl     = '<'
  | isGold        coord lvl     = '$'
  | isPotion      coord lvl     = '!'
  | isUpstairs    coord lvl     = '>'
  | isVillian     coord lvl     = 'v'
  | isWall        coord lvl     = '#'
  | isWeapon      coord lvl     = ')'
  | otherwise                   = ' '


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
  putChar 'v'
drawChar ')' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Cyan ]
  putChar ')'
drawChar '>' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Blue ]
  putChar '>'
drawChar '<' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Cyan ]
  putChar '<'
drawChar '\n' = do
  putChar '\n'
drawChar '+' = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Magenta ]
  putChar '+'
drawChar '-' = do
  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Yellow ]
  putChar '-'
drawChar '~' = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Green ]
  putChar '~'  
drawChar _ = do
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ]
  putChar ' '


drawCoord :: World -> Coord -> IO ()
drawCoord world coord = do
  uncurry (flip setCursorPosition) coord
  drawChar (coordToChar coord world) 
  
  
drawHero :: World -> IO ()
drawHero world
  | newPos == oldPos = return ()
  | otherwise        =  do
    drawCoord world newPos 
    drawCoord world oldPos
  where
    hero   = wHero world
    newPos = hCurrPos hero
    oldPos = hOldPos  hero
  
    
drawWorld :: World -> IO ()
drawWorld world = do
  setCursorPosition 0 0
  mapM_ drawChar (unlines chars)
  where
    lvl     = wLevel world
    (x',y') = lMax lvl
    chars   = [[coordToChar (x,y) world | x <- [0..x']]
                                        | y <- [0..y']]
