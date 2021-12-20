module AdventOfCode.Y2021.Day17 where

import AdventOfCode.Common.Grid (Coords)
import AdventOfCode.Common.Tuple
import AdventOfCode.Common.Util

import Control.Monad

import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

type Range = (Int, Int)

data TargetArea = TargetArea
   { xRange :: Range
   , yRange :: Range
   } deriving (Show, Eq)

--------------------------------------------------------------------------------

rangeCompare :: Int -> Range -> Ordering
rangeCompare x (a,b) = if x < a then LT else if b < x then GT else EQ

-- Computes maximum y position for given initial y velocity
-- / maximum x position for given initial x velocity
apex :: Int -> Int
apex n = (n * (n+1)) `div` 2

-- Computes the set of initial x velocities that can hit the target,
-- paired with the number of steps required to hit (min and max)
xVelocityRange :: TargetArea -> [(Int, Range)]
xVelocityRange target = let
   (minX, maxX) = xRange target
   a = head $ dropWhile (\x -> apex x < minX) [0..]
   path x = if x == 0 then [] else x : map (x+) (path (x-1))
   hitsTarget x = rangeCompare x (minX,maxX) == EQ
   makeBigger q x = if q == x then xXx_LARGE_NUMBER_xXx else q
   stepsToHitTarget x = case findIndices hitsTarget (path x) of
      { [] -> Nothing
      ; xs -> Just (head xs + 1, makeBigger (last xs + 1) x) }
   in catMaybes $ map (sequence . msnd stepsToHitTarget . two) [a..maxX]

xXx_LARGE_NUMBER_xXx = 1000000000

-- Find largest y such that sequence 0,y+1,2y+3,3y+6,... always contains a value in the target y range
maxYVelocity :: TargetArea -> Int
maxYVelocity target = subtract 1 $ negate $ fst $ yRange target

part1 :: TargetArea -> Int
part1 target = apex $ maxYVelocity target

--------------------------------------------------------------------------------

-- Computes the set of initial y velocities that can hit the target,
-- paired with the number of steps required to hit (min and max)
yVelocityRange :: TargetArea -> [(Int, Range)]
yVelocityRange target = aux $ maxYVelocity target where
   range = yRange target
   path y = 0 : map (y+) (path (y-1))
   
   aux :: Int -> [(Int, Range)]
   aux v = let
      p@(_:(_,p1):_) = zip [0..] $ path v
      hitSteps = map fst
         $ takeWhile (\(n,y) -> rangeCompare y range == EQ)
         $ dropWhile (\(n,y) -> rangeCompare y range == GT) p
      next = aux (v-1)
      -- if after first step, probe is below target area, then smaller velocities will also miss so stop searching here
      in if rangeCompare p1 range == LT
         then []
         else if hitSteps == []
            then next
            else (v, (head hitSteps, last hitSteps)) : next

hasOverlap :: Range -> Range -> Bool
hasOverlap (a,b) (c,d) = not (b < c || d < a)

successVelocities :: TargetArea -> [(Int, Int)]
successVelocities target = do
   (u, xSteps) <- xVelocityRange target
   (v, ySteps) <- yVelocityRange target
   guard $ hasOverlap xSteps ySteps
   return (u,v)

part2 :: TargetArea -> Int
part2 = length . successVelocities

--------------------------------------------------------------------------------

sampleInput :: TargetArea
sampleInput = TargetArea (20,30) (-10,-5)

myInput :: TargetArea
myInput = TargetArea (185,221) (-122,-74)
