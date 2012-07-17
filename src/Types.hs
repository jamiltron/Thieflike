module Types where

import qualified Data.Map as M


-- x/y coordinate
type Coord = (Int, Int)


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
data Hero = Hero { hEquip :: Weapon   -- weapon the hero weilds
                 , hGold  :: Int      -- purse
                 , hHP    :: Int      -- life
                 , hItems :: [Item]  -- inventory
                 , hCurrPos   :: Coord 
                 , hOldPos :: Coord }  -- location on map


data Input = Dir Direction
           | Exit


data Item = Arms  Weapon
          | Flask Potion


data Level = Level { lDepth      :: Int               -- depth of level
                   , lDownStairs :: Coord
                   , lUpStairs   :: Coord
                   , lGold       :: M.Map Coord Int   -- location and amount of $
                   , lWalls      :: M.Map Coord Char 
                   , lItems      :: M.Map Coord Item  
                   , lVillians   :: M.Map Coord Villian
                   , lMax        :: Coord}


-- consumable item, this implementation is based around a direction
-- numerical effect on who is consuming it, although that could certainly            
-- be abstracted away to make for a more general item
data Potion = Potion { pAmount :: Int
                     , pDesc   :: String   
                     , pEffect :: Effect }


-- the entire state of our game world
data World = World { wDepth     :: Int
                   , wHero      :: Hero
                   , wLevel     :: Level      -- current game level
                   , wLevels    :: [Level] }  -- all levels

                     
-- enemies, almost the same as heros except that they do not wield items
data Villian = Villian { vGold  :: Int 
                       , vHP    :: Int
                       , vItems :: [Item]
                       , vPos   :: Coord }


data Weapon = Weapon { wDesc   :: String
                     , wDamage :: Int 
                     , wToHit  :: Int }


emptyLevel = Level 0 (0,0) (0,0) M.empty M.empty M.empty M.empty (0,0)


-- bare fists/no weapon
fists    = Weapon "Bare fists" 0 0


-- a world with a basic hero, an empty level, and no items, gold, or villians
genesis  = World 0 commoner emptyLevel []


-- a basic hero with no weapon, no gold, 99 health, no items at (0,0)
commoner = Hero fists 0 99 [] (2, 2) (2, 2)