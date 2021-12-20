module AdventOfCode.Y2021.Day17 where

import AdventOfCode.Common.Grid (Coords)
import AdventOfCode.Common.Tuple

import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

type Range = (Int, Int)

data Probe = Probe
   { position :: Coords
   , velocity :: Coords
   } deriving (Show, Eq)

initialVelocity :: Coords -> Probe
initialVelocity v = Probe (0,0) v

data TargetArea = TargetArea
   { xRange :: Range
   , yRange :: Range
   } deriving (Show, Eq)

hits :: Probe -> TargetArea -> Bool
probe `hits` target = let
   (x,y) = position probe
   in and [rangeCompare x (xRange target) == EQ, rangeCompare y (yRange target) == EQ]

data Quadrant = X | A | B | C | D | E | F | G | H
   deriving (Show, Eq, Enum, Bounded)

{-
   A │ B │ C
  ───┼───┼───
   D │ X │ E
  ───┼───┼───
   F │ G │ H
X = target area
-}

data LaunchResult = Hit
   | Overshoot -- reduce x or y
   | Undershoot -- increase x or y
   | TooFast -- reduce x
   | Fallthrough -- falling too fast, change y
   | Short -- decrease x and maybe change y
   deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------

step :: Probe -> Probe
step probe = let
   (x,y) = position probe
   (u,v) = velocity probe
   pos' = (x + u, y + v)
   vel' = (u - signum u, v - 1)
   in probe { position = pos', velocity = vel' }

willHit :: Probe -> TargetArea -> Bool
probe `willHit` target
   | probe `hits` target = True
   | snd (position probe) < fst (yRange target) = False
   | fst (position probe) > snd (xRange target) = False
   | otherwise = (step probe) `willHit` target


quadrant :: TargetArea -> Coords -> Quadrant
quadrant target (x,y) = case (rangeCompare y $ yRange target, rangeCompare x $ xRange target) of
   (GT, LT) -> A
   (GT, EQ) -> B
   (GT, GT) -> C
   (EQ, LT) -> D
   (EQ, EQ) -> X
   (EQ, GT) -> E
   (LT, LT) -> F
   (LT, EQ) -> G
   (LT, GT) -> H

rangeCompare :: Int -> Range -> Ordering
rangeCompare x (a,b) = if x < a then LT else if b < x then GT else EQ

simulate :: Coords -> TargetArea -> LaunchResult
simulate v target = aux $ map (quadrant target . position) $ iterate step $ initialVelocity v
   where
   aux :: [Quadrant] -> LaunchResult
   aux (X:_) = Hit
   aux (A:C:_) = TooFast
   aux (A:E:_) = TooFast
   aux (A:F:_) = Fallthrough
   aux (A:G:_) = Fallthrough
   aux (A:H:_) = Fallthrough
   aux (B:C:_) = Overshoot
   aux (B:E:_) = Overshoot
   aux (B:G:_) = Fallthrough
   aux (B:H:_) = Fallthrough
   aux (C:E:_) = Overshoot
   aux (C:H:_) = Fallthrough
   aux (D:C:_) = TooFast
   aux (D:E:_) = TooFast
   aux (D:F:_) = Undershoot
   aux (D:G:_) = Undershoot
   aux (D:H:_) = TooFast
   aux (F:C:_) = TooFast
   aux (F:E:_) = TooFast
   aux (F:H:_) = TooFast
   aux (G:C:_) = Short
   aux (G:E:_) = Short
   aux (G:H:_) = Short
   aux (H:C:_) = Short
   aux (H:E:_) = Short
   aux (_:qs) = aux qs

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

xXx_LARGE_NUMBER_xXx = 1000000



--------------------------------------------------------------------------------

sampleInput :: TargetArea
sampleInput = TargetArea (20,30) (-10,-5)

myInput :: TargetArea
myInput = TargetArea (185,221) (-122,-74)
