module AdventOfCode.Y2021.Day11 where

import AdventOfCode.Common.Grid
import AdventOfCode.Common.List
import Data.Maybe

--------------------------------------------------------------------------------

data FlashingState = NotFlashing | Flashing | Finished
   deriving (Show, Eq, Ord)

data Octopus = Octopus
   { energyLevel :: Int
   , state :: FlashingState
   , flashes :: Int
   } deriving (Eq)

hasCriticalEnergy :: Octopus -> Bool
hasCriticalEnergy octo = energyLevel octo > 9

flashingQueued :: Octopus -> Bool
flashingQueued octo = hasCriticalEnergy octo && state octo == NotFlashing

isFlashing :: Octopus -> Bool
isFlashing octo = state octo == Flashing

instance Show Octopus where
   show octo
      | hasCriticalEnergy octo = "*"
      | otherwise = show (energyLevel octo)

instance Num Octopus where
   fromInteger n = Octopus (fromInteger n) NotFlashing 0
   Octopus e1 f1 t1 + Octopus e2 f2 t2 = Octopus (e1 + e2) (max f1 f2) (t1 + t2)

--------------------------------------------------------------------------------

step :: Grid Octopus -> Grid Octopus
step g = resetEnergy $ processFlashes $ fmap (+1) g where
   processFlashes :: Grid Octopus -> Grid Octopus
   processFlashes g = let
      hasUnprocessed = isJust $ findCoords flashingQueued g
      
      startFlashing octo = if flashingQueued octo
         then octo {state = Flashing, flashes = flashes octo + 1}
         else octo
      g1 = fmap startFlashing g
      
      propagateFlash p octo = octo {energyLevel = energyLevel octo + (count isFlashing $ neighboursD g1 p)}
      g2 = mapWithCoords propagateFlash g1
      
      finishFlashing octo = if state octo == Flashing
         then octo {state = Finished}
         else octo
      g3 = fmap finishFlashing g2
      
      in if hasUnprocessed then processFlashes g3 else g
   
   resetEnergy :: Grid Octopus -> Grid Octopus
   resetEnergy g = flip fmap g $ \octo -> if state octo == Finished
      then octo {state = NotFlashing, energyLevel = 0}
      else octo

doSteps :: Int -> Grid Octopus -> Grid Octopus
doSteps 0 = id
doSteps n = doSteps (n-1) . step

totalFlashes :: Grid Octopus -> Int
totalFlashes g = sum $ mapRows sum $ fmap flashes g

part1 :: Grid Octopus -> Int
part1 = totalFlashes . doSteps 100

--------------------------------------------------------------------------------

sampleInput :: Grid Octopus
sampleInput = Grid
   [ [5,4,8,3,1,4,3,2,2,3]
   , [2,7,4,5,8,5,4,7,1,1]
   , [5,2,6,4,5,5,6,1,7,3]
   , [6,1,4,1,3,3,6,1,4,6]
   , [6,3,5,7,3,8,5,4,7,8]
   , [4,1,6,7,5,2,4,6,4,5]
   , [2,1,7,6,8,4,1,7,2,1]
   , [6,8,8,2,8,8,1,1,3,4]
   , [4,8,4,6,8,4,8,5,5,4]
   , [5,2,8,3,7,5,1,5,2,6] ]

sampleInputSmall :: Grid Octopus
sampleInputSmall = Grid
   [ [1,1,1,1,1]
   , [1,9,9,9,1]
   , [1,9,1,9,1]
   , [1,9,9,9,1]
   , [1,1,1,1,1] ]

myInput :: Grid Octopus
myInput = Grid
   [ [6,7,8,8,3,8,3,4,3,6]
   , [5,5,2,6,8,2,7,4,4,1]
   , [4,5,8,2,4,3,5,8,6,6]
   , [5,1,5,2,5,4,7,2,7,3]
   , [3,7,4,6,4,3,3,6,2,1]
   , [2,4,6,5,1,4,5,3,6,5]
   , [6,3,2,4,8,8,7,1,2,8]
   , [8,5,3,7,5,5,8,7,4,5]
   , [4,7,1,8,4,2,7,5,6,2]
   , [2,2,8,3,3,2,4,7,4,6] ]
