module AdventOfCode.Y2020.Day23 where

import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

type Cup = Int
data CupCircle = CupCircle
   { cups :: [Cup]
   , pickup :: [Cup]
   , destination :: Cup
   } deriving (Eq, Show)

currentCup :: CupCircle -> Cup
currentCup = head . cups

allCups :: CupCircle -> [Cup]
allCups circle = cups circle ++ pickup circle

range :: CupCircle -> (Cup, Cup)
range circle = let all = allCups circle in (minimum all, maximum all)

--------------------------------------------------------------------------------

move :: CupCircle -> CupCircle
move = step4 . step3 . step2 . step1
   where
   step1 :: CupCircle -> CupCircle
   step1 circle = let
      (current:three, remainder) = splitAt 4 $ cups circle
      in CupCircle (current:remainder) three 0

   step2 :: CupCircle -> CupCircle
   step2 circle = let
      (lo,hi) = range circle
      nextCandidate c = if c == lo then hi else c - 1
      candidates = filter (\c -> not $ elem c $ pickup circle)
         $ tail
         $ iterate nextCandidate
         $ currentCup circle
      in circle {destination = head candidates}

   step3 :: CupCircle -> CupCircle
   step3 circle = let
      idx = fromJust $ findIndex (== destination circle) $ cups circle
      (a,b) = splitAt (idx + 1) $ cups circle
      in CupCircle (a ++ pickup circle ++ b) [] 0

step4 :: CupCircle -> CupCircle
step4 circle = circle {cups = tail (cups circle) ++ [head (cups circle)]}

doMove :: Int -> CupCircle -> CupCircle
doMove 0 = id
doMove n = doMove (n-1) . move

order :: CupCircle -> Int
order circle | currentCup circle /= 1 = order (step4 circle)
order circle = read $ concat $ map show $ tail $ cups circle

part1 :: CupCircle -> Int
part1 = order . doMove 100

--------------------------------------------------------------------------------

sampleInput :: CupCircle
sampleInput = CupCircle [3,8,9,1,2,5,4,6,7] [] 0

myInput :: CupCircle
myInput = CupCircle [9,5,2,4,3,8,7,1,6] [] 0
