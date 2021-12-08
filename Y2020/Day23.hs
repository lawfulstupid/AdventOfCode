{-# LANGUAGE BangPatterns #-}

module AdventOfCode.Y2020.Day23 where

import AdventOfCode.Common.Util
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

make :: [Cup] -> CupCircle
make cups = CupCircle cups [] 0

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
doMove 0 circle = circle
doMove n circle = let 
   !next = move circle
   in doMove (n-1) next

order :: CupCircle -> Int
order circle | currentCup circle /= 1 = order (step4 circle)
order circle = read $ concat $ map show $ tail $ cups circle

part1 :: [Cup] -> Int
part1 = order . doMove 100 . make

--------------------------------------------------------------------------------

make1000000 :: [Cup] -> CupCircle
make1000000 cups = make $ take 1000000 (cups ++ [maximum cups ..])

starCups :: CupCircle -> Int
starCups circle | currentCup circle /= 1 = order (step4 circle)
starCups circle = let 1:a:b:_ = cups circle in a * b

type Memory = [(CupCircle, Int)]

doMoveWithMem :: Int -> Memory -> CupCircle -> CupCircle
doMoveWithMem 0 mem circle = circle
doMoveWithMem n mem circle = case lookup circle mem of
   Nothing -> let !next = move circle in doMoveWithMem (n-1) ((circle,n):mem) next
   Just k -> interpolate mem n k
   
interpolate :: Memory -> Int -> Int -> CupCircle
interpolate m a b = let
   t = head $ dropWhile (<a) [0, b-a ..]
   in case lookup t $ map (\(x,y) -> (y,x)) m of
      Just x -> x
      Nothing -> error "idk"

part2 :: [Cup] -> Int
part2 = starCups . doMoveWithMem 10000000 [] . make1000000

--------------------------------------------------------------------------------

sampleInput :: [Cup]
sampleInput = [3,8,9,1,2,5,4,6,7]

myInput :: [Cup]
myInput = [9,5,2,4,3,8,7,1,6]
