module Types where

import qualified Data.Array.Unboxed as A
import qualified Data.Map as Map


-- x/y coordinate
type Coord = (Int, Int)


-- an unboxed array of (x,y) to ascii representation of a tile
type Level = A.UArray Coord Char


-- directions that heros or villians may move
data Direction = Up
               | Down
               | Left
               | Right


-- effects used in potions, could also be expanded for magic if that
-- was put into the game
data Effect = Heal
            | Harm


-- our brave protagonist
data Hero = Hero { hEquip :: Weapon   -- what weapon the hero weilds
                 , hGold  :: Int      -- purse
                 , hHP    :: Int      -- life
                 , hItems :: [Item]  -- inventory
                 , hPos   :: Coord }  -- location on map


data Input = Dir Direction
           | Exit


data Item = Arms  Weapon
          | Flask Potion


-- consumable item, this implementation is based around a direction
-- numerical effect on who is consuming it, although that could certainly            
-- be abstracted away to make for a more general item
data Potion = Potion { pAmount :: Int
                     , pDesc   :: String   
                     , pEffect :: Effect }


-- the entire state of our game world
data World = World { wGold      :: Map.Map Coord Int
                   , wHero      :: Hero
                   , wItems     :: Map.Map Coord Item
                   , wLevel     :: Level  
                   , wVillians  :: Map.Map Coord Villian }


-- generic enemies, almost the same as heros except that they do not
-- weild items
data Villian = Villian { vGold  :: Int 
                       , vHP    :: Int
                       , vItems :: [Item]
                       , vPos   :: Coord }


data Weapon = Weapon { wDesc   :: String
                     , wDamage :: Int 
                     , wToHit  :: Int }


-- bare fists/no weapon
fists    = Weapon "Knuckles" 0 0


-- a world with a basic hero, an empty level, and no items, gold, or villians
genesis  = World Map.empty commoner Map.empty stage1 Map.empty


-- a world consisting of the tile (0,0) and a hero
stage1   = A.array ((0,0), (0,0)) [((0,0), '@')]


-- a basic hero with no weapon, no gold, 99 health, no items at (0,0)
commoner = Hero fists 0 99 [] (0, 0)