module Types where

import qualified Data.Map as M


-- x/y coordinate
type Coord = (Int, Int)


-- armor provides a static defense in to-hit rolls
data Armor = Armor { aDefense :: Int 
                   , aDesc :: String }


-- directions that heros or villians may move
data Direction = Up
               | Down
               | Left
               | Right


-- could also be expanded to include secret doors
data Door = Closed
          | Open


-- effects used in potions, could also be expanded 
-- for magic if that was put into the game
data Effect = Heal
            | Harm


-- our brave protagonist
data Hero = Hero { hCurrPos :: Coord   -- current location on map
                 , hGold    :: Int     -- gold in coinpurse 
                 , hHP      :: Int     -- life
                 , hItems   :: [Item]  -- inventory
                 , hOldPos  :: Coord   -- previous location
                 , hWeild   :: Weapon  -- weapon hero is holding
                 , hWears   :: Armor } -- armor hero is wearing


data Input = Dir Direction
           | Exit


data Item = Arm  Armor
          | Pot  Potion
          | Weap Weapon


-- an entire map along with every item/feature on the map aside from the hero
data Level = Level { lDepth    :: Int                   -- depth of level
                   , lGold     :: M.Map Coord Int       -- pos and amount of $
                   , lItems    :: M.Map Coord Item      -- pos of items
                   , lMapped   :: M.Map Coord Bool
                   , lMax      :: Coord                 -- max x/y of level
                   , lTiles    :: M.Map Coord Tile      -- features
                   , lVillians :: M.Map Coord Villian } -- pos of enemies
                   


-- consumable item, based around a direct numerical effects
data Potion = Potion { pAmount :: Int
                     , pDesc   :: String   
                     , pEffect :: Effect }
              

data Stairs = Downstairs
            | Upstairs


-- the different kinds of flooring/furniture found throughout the dungeon
data Tile  = Acid 
           | Dr   Door 
           | St   Stairs
           | Wall


-- enemies, almost the same as heros except that they do not wield items
data Villian = Villian { vCurrPos :: Coord
                       , vGold    :: Int 
                       , vHP      :: Int
                       , vItems   :: [Item]
                       , vOldPos  :: Coord }


data Weapon = Weapon { wDamage :: Int     -- added to dmg rolls on hits
                     , wDesc   :: String  
                     , wToHit  :: Int }   -- added to to-hit rolls


-- the entire state of our game world
data World = World { wDepth  :: Int        -- current level depth
                   , wHero   :: Hero       -- the player
                   , wLevel  :: Level      -- current game level
                   , wLevels :: [Level] }  -- all levels


emptyLevel = Level { lDepth    = 0
                   , lGold     = M.empty
                   , lItems    = M.empty
                   , lMapped   = M.fromList [((0,0), True)]
                   , lMax      = (0,0)  
                   , lTiles    = M.empty
                   , lVillians = M.empty }


-- bare fists/no weapon
fists = Weapon 0 "Bare fists" 0


-- no armor
rags = Armor 0 "Rags"


-- a basic world used to start the game
genesis  = World { wDepth  = 0
           , wHero   = commoner  
           , wLevel  = emptyLevel
           , wLevels = [emptyLevel] }  -- all levels


-- a basic hero
commoner = Hero { hCurrPos = (1,1)
                , hGold   = 0  
                , hHP     = 10 
                , hItems  = [] 
                , hOldPos = (1,1)
                , hWeild  = fists
                , hWears  = rags }
