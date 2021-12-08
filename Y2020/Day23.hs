{-# LANGUAGE BangPatterns #-}

module AdventOfCode.Y2020.Day23 where

import AdventOfCode.Common.Util
import Data.List
import Data.Maybe

--------------------------------------------------------------------------------

type Cup = Int
data CupCircle = CupCircle
   { size :: Int
   , cups :: [Cup]
   } deriving (Eq)

instance Show CupCircle where
   show (CupCircle s cs) = show $ take s cs

currentCup :: CupCircle -> Cup
currentCup = head . cups

make :: [Cup] -> CupCircle
make cups = CupCircle (length cups) (loop cups)

loop :: [a] -> [a]
loop xs = xs ++ loop xs

--------------------------------------------------------------------------------

move :: CupCircle -> CupCircle
move circle = let
   !s = size circle
   (!current:three, remainder) = splitAt 4 $ cups circle
   !destination = head
      $ filter (\c -> not $ elem c three)
      $ map (\n -> 1 + (currentCup circle - n) `mod` s) [2..]
   !idx = fromJust $ findIndex (==destination) remainder
   (!a,b) = splitAt (idx+1) remainder
   in CupCircle s $ loop (take (s-1) (a ++ three ++ b) ++ [current])

step :: CupCircle -> CupCircle
step circle = circle {cups = tail (cups circle)}

doMove :: Int -> CupCircle -> CupCircle
doMove n circle = foldr (const move) circle [1..n]

order :: CupCircle -> Int
order circle = read $ foldMap show $ take (size circle - 1) $ tail $ dropWhile (/= 1) $ cups circle

part1 :: [Cup] -> Int
part1 = order . doMove 100 . make

--------------------------------------------------------------------------------

make1000000 :: [Cup] -> CupCircle
make1000000 cups = make $ take 1000000 (cups ++ [maximum cups + 1 ..])

starCups :: CupCircle -> Int
starCups circle = let 
   1:a:b:_ = dropWhile (/= 1) $ cups circle
   in a * b

part2 :: [Cup] -> Int
part2 = starCups . doMove 10000000 . make1000000

--------------------------------------------------------------------------------

sampleInput :: [Cup]
sampleInput = [3,8,9,1,2,5,4,6,7]

myInput :: [Cup]
myInput = [9,5,2,4,3,8,7,1,6]
