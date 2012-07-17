module Level where

import qualified Data.Map  as M
import qualified Data.List as L
import System.Console.ANSI

import Types

-- given a list of strings (assuming a text representation split on newlines)
-- return a level
strToLevel :: [String] -> Level
strToLevel str = foldl populate emptyLevel {lMax=maxXY} asciiMap
  where
    asciiMap = concat $ zipWith zip coords str
    coords   = [[(x, y) | x <- [0..]] | y <- [0..]]
    maxX     = maximum . map (fst . fst) $ asciiMap
    maxY     = maximum . map (snd . fst) $ asciiMap
    maxXY    = (maxX, maxY)
    populate lvl (coord, tile) =
      case tile of
        '#'   -> lvl { lWalls      = M.insert coord '#' (lWalls lvl) }
        '>'   -> lvl { lDownStairs = coord  }
        '<'   -> lvl { lUpStairs   = coord  }
        _     -> lvl


isWall coord lvl   = M.member coord (lWalls lvl)


isDStair coord lvl = coord == (lDownStairs lvl)


isUStair coord lvl = coord == (lUpStairs lvl)


isGold coord lvl = M.member coord (lGold lvl)


isVillian coord lvl = M.member coord (lVillians lvl)


isPotion coord lvl = case M.lookup coord (lItems lvl) of
  Just (Flask _) -> True
  _              -> False


isWeapon coord lvl = case M.lookup coord (lItems lvl) of
  Just (Arms _) -> True
  _             -> False
                        
            
map1   = [ "##############"
         , "#            #          ######"
         , "#            ############    #"
         , "#    >                       #"
         , "#            ############    #"
         , "#            #          #    #"
         , "#            #          # <  #"
         , "##############          ######" ]
            

level1 = strToLevel map1