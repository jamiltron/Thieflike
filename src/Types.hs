module Types where

import Data.Array.Unboxed


type Coord = (Int, Int)

type Level = UArray Coord Char

data Direction = Up
               | Down
               | Left
               | Right
                 
data Input = Dir Direction
           | Exit

data Hero = Hero { hPos  :: Coord
                 , hGold :: Int
                 , hHP   :: Int }

data World = World { wHero  :: Hero
                   , wLevel :: Level }
             
genesis = (World commoner (array ((0,0), (0,0)) [((0,0), '@')]))


commoner = (Hero (0,0) 0 99)