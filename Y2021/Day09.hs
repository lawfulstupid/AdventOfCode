module AdventOfCode.Y2021.Day9 where

import AdventOfCode.Common.Grid
import Data.Maybe

--------------------------------------------------------------------------------

lowPoints :: Grid Int -> [Int]
lowPoints g = let
   isLowPoint p x = all (x <) $ neighbours g p
   transform p x = if isLowPoint p x then Just x else Nothing
   in catMaybes $ concat $ unpack $ mapWithCoords transform g

riskLevel :: Int -> Int
riskLevel x = x + 1

part1 :: Grid Int -> Int
part1 = sum . map riskLevel . lowPoints

--------------------------------------------------------------------------------

sampleInput :: Grid Int
sampleInput = Grid
   [ [2,1,9,9,9,4,3,2,1,0]
   , [3,9,8,7,8,9,4,9,2,1]
   , [9,8,5,6,7,8,9,8,9,2]
   , [8,7,6,7,8,9,6,7,8,9]
   , [9,8,9,9,9,6,5,6,7,8] ]

myInput :: Grid Int
myInput = Grid
   [ [7,8,5,7,6,7,9,8,7,6,5,4,5,9,8,7,6,4,3,2,5,6,7,8,9,7,6,7,8,9,4,5,9,2,1,2,9,8,7,5,4,3,2,3,4,5,6,7,8,9,8,7,5,4,3,2,3,4,5,9,8,7,5,4,5,6,7,8,9,2,3,4,9,9,6,4,3,4,5,9,4,3,2,1,0,1,2,7,8,9,9,4,3,4,9,5,4,3,4,5]
   , [6,7,4,6,5,6,7,9,8,7,7,9,6,9,9,7,6,6,4,1,4,6,7,8,9,6,5,6,9,5,3,9,8,9,3,4,9,7,5,4,3,2,1,2,3,4,7,9,9,9,9,8,9,9,9,1,0,9,9,8,9,8,4,3,4,6,7,9,9,3,4,9,8,8,9,5,6,5,9,8,5,4,3,2,2,3,4,5,6,7,8,9,4,9,8,9,3,2,3,3]
   , [5,4,3,4,4,5,8,9,9,8,9,8,9,8,7,6,5,4,3,2,3,4,5,9,9,5,4,5,8,9,9,8,7,8,9,9,8,6,5,4,3,2,0,1,2,3,6,7,8,9,9,9,8,7,8,9,9,8,9,7,5,4,3,2,3,5,8,7,8,9,9,8,7,7,8,9,7,9,8,7,6,5,5,6,3,4,5,6,8,9,9,9,9,8,7,8,9,1,0,2]
   , [4,5,2,3,3,4,7,9,9,9,9,7,8,9,8,9,8,5,4,8,4,5,6,7,8,9,3,8,7,9,8,7,6,7,9,9,9,8,6,5,4,3,1,3,3,4,5,6,7,8,9,8,9,6,5,9,8,7,6,5,4,3,2,1,3,4,5,6,7,8,9,6,5,6,7,8,9,6,9,8,8,7,6,7,8,5,6,7,9,4,3,9,8,7,6,7,8,9,1,3]
   , [3,2,1,0,1,5,6,8,9,9,8,6,6,6,9,8,7,6,6,7,8,9,7,8,9,3,2,5,6,7,9,6,5,6,7,9,9,8,7,6,5,6,5,4,5,7,6,7,8,9,6,7,9,9,4,3,9,8,7,7,5,2,1,0,1,2,3,8,8,9,3,2,3,5,8,9,9,4,3,9,9,8,7,8,9,6,8,9,6,5,9,8,7,6,5,6,8,9,9,5]
   , [6,5,2,1,2,3,8,7,8,9,6,5,4,5,6,9,9,7,7,8,9,9,9,9,9,7,3,4,5,6,9,5,4,5,6,8,9,9,8,7,8,9,7,5,6,8,9,8,9,3,5,6,7,8,9,2,1,9,8,8,5,4,2,4,3,4,4,9,9,4,2,1,4,5,6,7,8,9,2,1,2,9,8,9,9,8,9,8,7,9,8,7,6,5,4,5,6,7,8,9]
   , [7,4,3,4,3,4,5,6,9,9,9,4,3,4,5,6,9,9,8,9,8,8,9,9,7,6,4,5,6,9,8,7,3,4,5,6,7,8,9,9,9,9,8,6,8,9,3,9,6,4,6,7,8,9,6,3,2,3,9,6,5,4,3,5,6,5,6,7,8,9,1,0,5,8,7,8,9,6,4,2,9,8,9,0,1,9,4,9,8,9,9,8,7,6,5,6,9,8,9,3]
   , [6,5,7,5,6,5,6,8,9,7,8,9,9,5,6,9,8,7,9,5,6,7,9,9,8,7,8,7,9,8,7,6,4,5,6,7,8,9,5,4,3,4,9,8,9,1,2,9,6,5,9,8,9,8,5,4,3,4,5,9,7,5,4,5,6,6,9,9,9,3,2,1,6,7,8,9,9,9,5,9,8,7,9,1,2,9,3,4,9,7,8,9,8,7,6,7,9,9,3,2]
   , [7,6,8,9,7,8,7,8,9,6,7,9,8,9,7,9,9,6,7,4,6,7,8,9,9,8,9,8,9,9,8,7,7,6,7,8,9,7,6,3,2,1,2,9,8,9,9,8,7,6,7,9,8,7,6,5,4,5,6,9,8,7,5,6,7,7,8,9,5,4,3,2,3,8,9,9,9,8,9,8,7,6,8,9,9,8,9,9,8,6,7,8,9,9,8,8,9,3,2,1]
   , [8,7,9,9,8,9,8,9,6,5,9,8,7,8,9,8,6,5,2,3,7,9,9,9,7,9,9,9,3,1,9,8,8,7,8,9,8,7,6,5,3,4,3,6,7,9,9,9,8,9,9,3,9,9,7,6,7,7,7,8,9,7,6,7,8,8,9,9,9,7,4,3,4,5,6,8,9,7,9,7,8,5,6,7,9,7,5,9,8,7,8,9,9,8,9,9,5,4,3,2]
   , [9,8,9,0,9,6,9,7,5,4,3,4,6,7,8,9,7,4,3,4,9,8,9,8,5,6,8,9,9,2,9,9,9,8,9,6,9,8,7,6,9,5,4,5,6,7,8,9,9,6,1,2,3,9,8,7,9,8,9,9,9,8,7,8,9,9,7,9,8,6,5,4,5,6,7,9,7,6,5,6,5,4,3,4,9,6,4,3,9,8,9,8,7,6,7,9,9,5,4,6]
   , [6,9,2,1,3,4,9,9,4,3,2,3,5,9,9,7,6,5,5,6,9,7,6,7,4,5,7,9,8,9,8,7,8,9,4,5,6,9,8,9,8,9,5,6,7,8,9,9,6,5,4,3,4,5,9,9,8,9,9,8,9,9,8,9,8,7,6,5,9,7,8,7,6,7,8,9,6,5,4,3,2,1,2,3,8,9,5,2,0,9,9,8,6,5,6,8,8,9,5,7]
   , [5,4,3,2,6,7,7,8,9,0,1,9,6,8,9,8,7,6,6,9,8,7,5,4,3,4,9,8,7,6,6,5,6,4,3,4,9,9,9,8,7,8,9,9,8,9,9,8,9,9,6,5,5,6,9,8,7,8,9,7,9,9,9,8,7,6,5,4,9,8,9,9,7,8,9,9,7,6,6,6,1,0,1,7,7,8,9,3,1,9,8,7,6,4,5,6,7,8,9,8]
   , [6,5,6,3,4,5,6,7,8,9,9,8,9,9,2,9,8,7,7,9,9,9,4,3,2,3,4,9,6,5,4,3,2,3,2,9,8,9,8,7,6,7,8,9,9,6,5,6,9,8,9,6,6,9,8,7,6,7,7,6,7,8,9,8,7,5,4,3,4,9,8,9,8,9,4,9,8,7,7,5,3,2,4,5,6,7,8,9,2,9,9,8,5,3,4,6,6,7,8,9]
   , [9,8,7,4,5,6,9,8,9,9,8,7,8,9,1,2,9,8,8,9,9,8,9,2,1,2,3,4,9,6,5,2,1,0,1,9,7,6,5,6,5,8,8,9,2,3,4,5,6,7,8,9,9,8,7,6,5,4,3,5,6,7,8,9,8,9,5,4,5,6,7,8,9,2,3,4,9,9,8,7,4,3,7,6,7,8,9,9,9,8,5,4,3,2,3,4,5,7,8,9]
   , [5,9,8,6,7,9,8,9,9,8,7,6,7,8,9,3,4,9,9,9,8,7,9,9,0,1,5,9,8,7,4,3,4,3,2,9,9,7,4,3,4,8,6,7,9,4,5,7,8,9,9,4,5,9,6,5,4,3,2,4,6,8,9,9,9,7,6,5,6,7,8,9,0,1,2,6,7,8,9,6,5,4,8,7,9,9,9,8,9,7,6,5,4,5,4,5,6,7,8,9]
   , [4,9,8,7,9,8,7,9,7,9,8,7,8,9,8,9,5,9,8,8,9,6,8,8,9,3,4,6,9,8,9,5,5,4,9,8,7,6,3,2,3,7,5,6,8,9,6,8,9,7,5,3,9,8,7,8,5,4,3,4,5,6,7,8,9,9,7,6,8,8,9,2,1,3,4,7,8,9,9,7,6,7,9,8,9,9,8,7,9,8,7,6,9,8,5,7,8,8,9,9]
   , [3,2,9,9,8,7,6,7,6,8,9,8,9,5,6,9,9,8,7,6,7,5,6,7,9,9,5,6,9,8,7,6,6,5,6,9,6,5,2,1,2,3,4,6,7,8,9,9,9,7,6,2,5,9,8,9,8,5,4,5,7,8,9,9,9,9,8,7,8,9,4,3,5,4,8,9,9,6,5,9,8,8,9,9,9,8,7,5,6,9,8,9,8,7,6,7,9,9,9,8]
   , [2,1,9,8,7,6,5,6,5,6,7,9,5,4,9,8,7,9,5,4,5,4,5,6,7,8,9,7,8,9,8,7,9,6,9,8,5,4,3,2,3,4,5,9,8,9,6,8,9,6,5,3,4,5,9,8,7,6,5,7,8,9,9,9,9,8,9,8,9,9,8,7,6,5,6,7,8,9,4,5,9,9,9,9,8,7,6,4,5,2,9,4,9,8,7,8,9,7,6,7]
   , [4,9,8,7,6,5,4,3,4,5,6,9,4,3,4,9,6,5,4,3,2,3,4,7,9,9,8,9,9,9,9,9,8,9,8,7,6,5,6,8,6,5,6,7,9,6,5,7,7,9,9,4,9,6,7,9,8,9,7,8,9,7,8,9,8,6,5,9,5,6,9,8,9,9,7,8,9,5,3,4,5,6,9,8,9,8,4,3,2,1,3,3,5,9,8,9,5,4,5,6]
   , [5,6,9,8,9,6,3,2,3,6,5,8,9,2,9,8,7,6,5,4,7,4,5,8,9,7,6,7,8,9,8,7,6,7,9,8,7,8,9,9,9,7,8,9,6,5,4,6,6,7,8,9,8,7,8,9,9,9,8,9,6,6,7,8,9,5,4,5,4,5,7,9,2,9,8,9,4,3,2,3,4,9,8,7,6,5,4,3,1,0,1,2,3,4,9,7,6,5,6,9]
   , [7,9,8,7,8,9,2,1,2,3,4,7,8,9,2,9,8,9,8,7,6,5,6,7,8,9,4,5,6,9,8,6,5,6,8,9,8,9,2,3,9,8,9,6,5,4,2,4,5,6,9,9,9,8,9,8,6,5,9,3,4,5,6,9,5,3,2,2,3,4,8,9,1,0,9,4,3,1,0,1,2,3,9,9,8,7,5,3,2,1,9,7,5,9,8,9,7,6,7,8]
   , [9,8,7,6,4,3,1,0,1,2,5,6,7,9,1,2,9,9,9,8,7,6,7,8,9,2,3,4,9,8,7,7,4,5,7,6,9,9,4,4,5,9,8,7,6,5,6,5,6,8,9,9,9,9,2,9,9,4,3,2,5,3,7,8,9,3,1,0,4,6,9,7,9,1,9,5,4,6,5,2,5,5,9,9,9,6,5,4,3,9,8,9,9,8,7,8,9,9,8,9]
   , [2,9,9,8,5,4,2,1,2,3,8,7,8,9,0,1,9,8,7,9,8,7,8,9,2,1,2,9,8,7,6,4,3,4,6,5,7,8,9,5,6,7,9,8,7,6,7,8,7,9,9,8,9,0,1,9,8,9,1,0,1,2,8,9,5,4,2,1,2,3,4,6,8,9,8,7,6,5,4,3,4,9,8,9,8,7,6,5,9,8,7,8,9,9,6,9,9,9,9,7]
   , [0,1,2,9,6,5,6,9,3,9,9,8,9,2,1,2,3,9,6,5,9,8,9,4,3,0,1,2,9,6,5,4,2,1,3,4,7,8,9,6,8,9,0,9,8,7,8,9,8,9,8,7,8,9,9,8,7,9,3,1,2,3,4,8,9,5,3,2,3,5,7,7,9,8,9,9,7,9,9,9,9,8,7,6,9,8,7,9,8,9,5,4,4,6,5,9,9,8,7,6]
   , [1,2,9,8,7,8,9,8,9,8,9,9,4,3,2,4,9,8,7,4,3,9,9,8,4,3,2,9,8,7,6,5,3,2,4,5,6,7,9,7,9,5,1,2,9,8,9,8,9,8,9,6,7,9,8,7,6,8,9,9,3,5,6,7,8,9,4,3,4,5,6,9,8,7,9,9,9,8,7,8,5,9,8,9,7,9,9,8,6,5,4,3,2,3,4,7,8,9,6,5]
   , [2,3,4,9,8,9,8,7,6,7,9,6,5,7,3,9,8,7,6,5,1,9,8,7,6,5,4,5,9,9,7,5,4,4,5,8,7,8,9,8,9,4,2,3,4,9,8,7,6,6,4,5,9,8,7,6,5,6,7,8,9,9,7,8,9,9,9,4,5,6,9,9,8,6,8,8,9,7,6,5,4,5,9,7,6,5,6,9,7,8,5,4,3,4,5,6,7,8,9,4]
   , [6,5,5,6,9,8,7,6,5,7,8,9,9,8,4,6,9,8,7,3,2,6,9,8,7,6,5,6,7,8,9,6,6,5,6,8,8,9,3,9,9,9,3,4,9,8,9,6,5,5,3,5,6,9,8,5,4,5,6,7,8,9,8,9,8,9,8,9,6,9,8,8,7,5,6,7,9,9,7,4,3,7,8,9,5,4,5,9,8,7,6,7,4,5,6,7,8,9,1,2]
   , [7,6,7,9,8,7,6,5,4,6,7,9,8,9,5,7,8,9,9,6,3,4,8,9,8,7,8,7,8,9,9,8,7,8,9,9,9,1,2,3,9,8,9,9,8,7,8,5,4,3,2,4,9,8,7,4,3,5,7,8,9,8,9,8,7,6,7,8,9,8,7,6,5,4,3,4,5,8,9,5,4,6,9,4,2,3,4,5,9,9,8,8,5,8,9,8,9,9,2,3]
   , [9,7,9,8,7,6,5,4,3,5,6,7,7,9,9,8,9,9,8,5,4,6,7,9,9,8,9,8,9,9,9,9,8,9,9,3,2,0,1,9,8,7,6,7,9,6,5,4,3,2,1,9,8,7,6,5,6,6,7,9,6,6,4,9,6,5,6,9,8,7,6,5,4,3,2,3,5,7,8,9,6,7,9,2,1,9,5,9,2,1,9,9,6,7,8,9,5,8,9,4]
   , [9,8,9,9,9,8,5,3,2,4,5,7,6,7,8,9,9,8,7,6,5,6,7,8,9,9,6,9,7,8,9,9,9,9,8,9,3,9,9,8,7,6,5,9,8,7,6,3,2,1,0,2,9,8,9,6,7,8,8,9,5,4,3,2,3,4,7,9,7,6,5,4,3,4,1,3,4,8,9,9,7,8,9,4,9,8,9,8,9,3,9,8,7,9,9,7,6,7,8,9]
   , [5,9,9,9,7,6,4,4,1,2,3,4,5,7,8,9,9,9,9,9,7,8,8,9,8,6,5,8,6,7,9,8,9,9,7,8,9,8,9,7,5,4,4,5,9,8,5,4,3,2,1,2,3,9,8,7,8,9,9,7,6,5,4,1,2,3,9,8,9,7,6,3,2,1,0,1,2,5,7,8,9,9,9,9,8,6,7,7,8,9,9,9,8,9,9,9,9,9,9,2]
   , [4,9,9,8,7,4,3,1,0,1,7,8,9,8,9,9,9,9,9,9,8,9,9,9,9,5,4,3,4,9,8,7,9,8,6,7,6,7,8,9,4,3,2,9,8,7,6,5,4,3,5,3,4,5,9,8,9,9,9,8,7,9,8,2,3,4,9,7,9,8,5,4,3,2,1,2,3,4,5,9,4,5,9,8,9,5,5,6,7,8,9,9,9,9,9,8,7,8,9,0]
   , [9,8,9,9,6,5,4,3,1,3,5,9,9,9,9,9,8,9,8,7,9,8,9,9,8,7,6,4,9,8,7,6,7,6,5,6,5,8,7,8,9,4,1,2,9,9,7,6,5,4,7,8,9,6,7,9,9,9,9,9,9,8,6,4,5,9,8,6,8,9,9,5,5,3,2,3,9,6,9,8,9,9,8,7,6,4,3,5,6,7,8,9,9,9,8,7,6,7,8,9]
   , [9,7,9,8,7,7,4,3,2,3,4,5,8,9,9,8,7,8,9,6,5,7,8,6,9,8,7,9,9,9,6,5,4,3,4,5,4,5,6,7,8,9,0,1,2,9,8,7,6,5,6,7,8,9,8,9,9,8,9,9,9,7,6,5,9,8,7,5,7,7,8,9,6,5,3,9,8,9,8,7,8,8,9,8,7,5,4,8,7,8,9,4,9,8,7,6,5,4,1,2]
   , [7,6,5,9,8,8,5,4,5,7,6,6,7,8,9,7,6,9,7,5,4,6,5,5,6,9,9,8,9,9,9,6,5,2,1,2,3,6,7,8,9,2,1,2,3,6,9,8,7,6,7,8,9,9,9,8,5,7,8,9,9,8,7,9,8,7,5,4,7,6,7,8,9,7,9,8,7,6,7,6,8,7,8,9,8,9,5,9,8,9,2,3,4,9,9,7,4,3,0,1]
   , [8,9,6,9,9,8,6,5,6,9,7,8,9,9,3,4,5,8,9,4,3,2,3,4,5,9,8,7,8,9,8,8,4,3,0,1,6,7,8,9,4,3,2,3,4,5,6,9,8,9,8,9,9,9,9,6,4,5,9,8,9,9,9,8,7,6,4,3,2,5,6,7,8,9,9,9,6,5,4,5,7,6,7,8,9,7,6,7,9,2,1,2,9,8,7,6,5,2,1,3]
   , [9,8,7,8,9,9,7,6,7,9,8,9,9,8,4,5,6,7,8,9,2,1,2,3,9,8,9,6,9,8,7,6,5,2,1,4,5,6,8,9,5,4,3,4,5,6,7,8,9,4,9,5,8,7,8,9,9,9,8,7,9,9,9,9,8,7,5,4,3,4,7,8,9,9,9,8,7,9,3,4,3,4,5,9,9,8,9,8,9,3,2,3,4,9,6,5,4,3,2,4]
   , [7,9,8,9,8,9,8,9,8,9,9,9,8,7,5,6,7,8,9,2,1,0,1,9,8,7,6,5,6,9,8,6,5,3,2,3,5,6,7,8,9,6,4,5,6,7,9,9,4,3,2,4,5,6,7,9,8,9,7,6,7,8,7,8,9,9,6,5,4,5,8,9,8,9,8,7,6,7,2,1,2,5,6,7,8,9,9,9,6,5,4,5,9,8,7,6,5,4,5,5]
   , [6,7,9,6,7,8,9,9,9,7,5,6,9,8,7,9,8,9,8,4,3,1,2,5,9,8,7,4,7,8,9,8,6,4,3,5,6,7,8,9,9,8,6,7,8,8,9,6,5,2,1,2,7,8,9,8,7,7,6,5,3,4,5,6,7,8,9,6,5,6,9,9,7,6,9,8,5,4,3,2,3,4,7,9,9,9,9,8,7,6,5,9,9,9,9,8,6,7,7,6]
   , [4,3,4,5,6,7,9,9,8,6,4,5,6,9,9,8,9,8,7,6,5,2,3,4,9,9,8,9,8,9,9,9,7,5,4,5,6,7,8,9,9,9,8,8,9,9,9,7,6,5,2,3,4,9,9,7,6,5,4,4,2,3,4,6,6,7,8,9,6,8,9,8,9,5,4,9,7,6,5,7,4,5,6,7,8,9,9,9,8,7,9,8,9,5,6,9,7,9,8,7]
   , [3,2,3,9,7,9,8,7,6,5,3,4,5,9,8,7,8,9,9,7,6,7,8,9,8,9,9,1,9,9,8,9,9,6,7,6,7,8,9,9,8,9,9,9,7,6,5,9,5,4,3,4,5,9,8,6,5,4,3,2,1,2,4,5,5,6,7,8,9,9,6,7,9,9,3,9,8,7,6,9,6,7,9,8,9,9,9,9,9,9,8,7,5,4,5,6,9,9,9,8]
   , [4,9,9,8,9,9,8,7,4,3,2,2,9,8,7,6,7,8,8,9,7,9,9,7,7,8,9,2,9,8,7,9,8,7,9,7,9,9,9,8,7,8,9,9,8,7,9,8,9,5,4,5,6,7,9,8,6,5,4,1,0,1,2,3,4,5,6,9,5,4,5,6,7,8,9,9,9,8,7,8,9,9,7,9,8,7,8,9,4,3,9,8,6,5,6,7,8,9,9,9]
   , [9,8,6,7,8,9,7,6,5,1,0,1,3,9,5,4,5,6,7,8,9,8,7,6,5,6,7,9,8,9,6,7,9,8,9,8,9,8,8,7,6,7,9,9,9,9,8,7,8,9,5,6,7,9,8,7,5,4,3,2,1,2,3,6,5,6,9,4,3,2,9,7,8,9,9,9,8,9,9,9,9,9,6,5,7,6,7,8,9,1,3,9,7,6,7,8,9,5,8,8]
   , [8,7,5,6,7,8,9,5,4,3,1,2,9,8,7,6,7,7,9,9,9,9,9,7,6,7,9,8,7,9,5,4,5,9,9,9,8,7,5,6,5,6,8,8,9,8,7,6,7,8,9,8,8,9,9,8,9,5,4,3,5,3,4,5,6,7,8,9,9,9,8,9,9,9,8,7,6,5,2,1,9,8,7,4,5,4,5,9,9,0,4,6,9,8,8,9,3,4,6,7]
   , [7,5,4,5,6,7,9,9,5,6,7,8,9,9,9,7,8,8,9,9,9,9,9,8,9,9,8,9,6,8,9,9,7,9,8,7,6,5,4,5,4,5,9,7,9,9,9,5,6,7,9,9,9,8,9,8,7,6,5,4,6,7,8,9,9,8,9,9,8,9,7,9,9,8,7,6,5,4,3,9,8,7,8,3,2,3,4,5,7,9,9,7,9,9,9,4,2,3,4,3]
   , [4,2,3,4,5,6,7,8,9,7,8,9,3,4,9,8,9,9,9,9,8,9,8,9,8,9,7,6,5,6,7,8,9,9,9,8,7,4,3,4,3,4,5,6,7,8,9,4,5,6,7,8,9,7,8,9,8,7,6,7,8,8,9,3,2,9,9,8,7,5,6,7,8,9,8,7,9,5,9,8,7,6,4,2,1,2,3,4,6,7,8,9,8,9,4,3,1,0,1,2]
   , [3,1,2,3,4,9,9,9,9,8,9,1,2,9,8,9,5,6,8,9,7,6,7,9,7,9,8,5,4,9,8,9,9,8,6,5,4,3,2,1,2,3,6,7,8,9,1,3,6,7,8,9,3,5,7,8,9,9,8,9,9,9,3,2,1,2,9,6,5,4,5,6,7,9,9,9,8,9,9,9,8,7,2,1,0,1,2,4,5,6,9,8,7,8,9,4,2,1,3,4]
   , [4,5,3,4,5,8,9,9,9,9,1,0,9,8,7,5,4,7,7,8,9,5,9,8,6,5,4,3,3,8,7,9,9,8,7,6,7,4,1,0,1,4,7,9,9,1,0,4,5,9,9,5,2,1,2,9,9,9,9,7,8,9,9,4,2,9,8,9,4,3,4,7,8,9,9,8,7,9,9,9,9,5,3,4,1,2,3,4,6,7,8,9,6,7,8,9,9,2,4,5]
   , [6,6,4,5,6,7,9,9,8,9,2,1,2,9,5,4,3,5,6,9,5,4,3,9,7,4,3,2,1,3,6,7,9,9,8,7,8,9,2,4,7,5,6,7,8,9,1,2,7,8,9,4,3,2,9,8,8,9,7,6,5,7,8,9,9,8,7,9,1,2,3,4,5,7,8,9,6,7,8,9,9,8,5,3,2,3,9,9,9,8,9,4,5,6,8,9,8,9,5,6]
   , [8,7,6,7,7,8,9,8,7,8,9,9,9,8,6,4,1,6,7,8,9,1,2,9,8,5,4,3,2,4,5,6,7,8,9,8,9,8,6,5,6,6,7,9,9,3,2,3,6,7,8,9,4,9,8,7,6,5,6,3,4,6,9,8,9,7,6,7,9,3,4,5,6,8,9,6,5,6,7,9,9,7,5,4,3,9,8,7,8,9,4,3,4,5,9,8,7,8,9,7]
   , [9,8,7,8,8,9,6,5,6,9,9,8,9,8,7,3,2,9,8,9,9,9,9,8,7,6,5,9,6,5,6,7,8,9,8,9,9,9,7,6,7,9,8,9,5,4,3,4,5,6,7,8,9,9,9,6,5,4,3,2,3,5,6,7,8,9,5,6,8,9,5,9,7,9,2,5,4,5,7,9,8,7,6,5,9,8,7,6,7,8,9,2,3,9,9,7,6,7,8,9]
   , [9,9,8,9,9,5,6,4,6,9,8,7,8,9,9,9,3,4,9,8,9,8,7,9,8,7,9,8,7,6,7,8,9,5,7,8,9,9,8,9,8,9,9,7,6,5,7,6,6,9,8,9,9,9,8,7,6,5,2,1,2,6,7,8,9,5,4,5,7,8,9,8,9,0,1,2,3,4,9,9,9,9,7,6,9,9,8,5,6,7,9,1,9,8,7,6,5,6,7,8]
   , [8,7,9,7,6,4,3,3,5,9,7,6,7,8,9,8,9,5,6,7,8,9,6,5,9,8,9,9,8,8,9,9,5,4,5,7,8,9,9,9,9,9,9,8,7,9,8,7,8,9,9,9,9,7,9,9,7,5,3,2,3,4,6,7,9,5,3,4,6,8,9,7,8,9,2,3,4,9,8,9,9,9,8,9,8,6,5,4,5,6,8,9,9,9,6,5,4,3,5,6]
   , [7,6,5,9,4,5,1,2,9,8,6,5,6,9,8,7,9,9,9,8,9,6,5,4,3,9,4,5,9,9,9,8,7,6,8,9,9,9,9,8,7,9,9,9,8,9,9,8,9,6,7,9,8,6,5,9,8,6,6,5,4,5,7,8,9,2,1,2,3,4,5,6,9,6,5,4,9,8,7,8,9,9,9,8,7,6,4,3,2,4,7,8,9,9,8,7,5,2,6,8]
   , [8,9,4,3,2,1,0,9,8,7,6,4,9,8,7,6,7,8,9,9,8,7,9,6,9,3,2,4,5,6,7,9,8,7,8,9,9,9,7,9,5,6,9,8,9,4,3,9,4,5,9,7,6,5,4,9,8,7,7,8,5,6,7,9,2,1,0,4,5,6,6,7,8,9,6,9,9,7,6,7,8,9,9,9,9,5,3,2,1,2,6,7,9,9,9,9,4,3,4,5]
   , [9,8,7,4,3,4,9,8,7,6,4,3,6,5,4,5,8,9,9,9,9,9,8,9,8,9,0,1,2,3,4,5,9,8,9,9,8,7,6,7,4,9,8,7,6,5,2,1,3,9,8,6,5,4,3,4,9,9,8,9,8,7,8,9,4,3,1,5,6,7,8,9,9,7,9,8,7,6,5,6,7,8,9,9,8,6,7,3,0,3,5,6,9,8,7,6,5,5,6,7]
   , [8,7,6,5,4,5,6,9,8,5,3,2,1,4,3,4,5,7,8,9,9,9,7,6,7,8,9,2,3,4,5,6,7,9,9,8,7,6,5,6,3,4,9,9,5,4,3,2,3,9,9,7,4,3,2,3,4,5,9,9,9,8,9,6,5,4,2,3,9,8,9,2,7,6,7,9,8,7,6,7,8,9,9,8,7,6,6,2,1,2,9,9,9,9,8,9,8,6,8,9]
   , [9,8,7,6,5,6,7,9,8,4,3,1,0,1,2,3,6,7,8,9,9,8,7,5,6,7,8,9,4,5,9,7,8,9,8,9,7,5,4,3,2,3,4,9,6,5,5,4,9,8,7,6,5,4,1,0,6,9,8,9,9,9,8,7,6,6,5,4,8,9,3,1,2,5,6,8,9,9,9,8,9,9,8,7,6,5,4,3,2,9,8,7,8,9,9,5,9,7,9,9]
   , [6,9,9,8,7,7,9,8,7,6,3,2,1,2,3,4,7,8,9,9,8,7,5,4,5,6,7,8,9,9,8,9,9,9,7,8,9,7,3,2,1,7,9,8,7,8,6,9,9,9,8,7,4,3,2,1,9,8,7,8,9,9,9,8,7,9,6,5,6,9,1,0,3,4,5,9,8,7,8,9,4,4,9,9,8,6,5,4,3,9,7,6,9,9,6,4,9,8,9,9]
   , [5,3,2,9,8,9,9,9,6,5,4,3,5,3,4,5,6,7,9,8,7,6,4,3,4,5,8,9,2,3,7,9,9,8,6,9,8,9,4,3,4,6,7,9,8,9,9,8,9,9,9,7,5,4,3,9,8,7,6,7,8,8,9,9,8,9,8,6,7,8,9,2,3,9,9,9,9,6,5,4,3,2,1,3,9,8,6,5,9,8,7,5,7,8,9,3,2,9,7,8]
   , [9,2,1,2,9,9,9,8,7,6,5,4,7,6,5,6,9,8,9,8,6,4,3,2,3,4,8,9,4,5,6,9,8,7,5,6,7,8,9,4,5,8,8,9,9,9,8,7,8,9,9,8,6,5,9,8,7,6,5,4,5,7,9,9,9,5,9,9,8,9,5,4,9,8,8,9,8,7,6,7,4,4,0,1,4,9,7,9,8,7,6,4,6,7,8,9,4,5,6,7]
   , [8,9,0,1,9,8,7,9,8,7,9,5,6,7,8,7,9,9,8,7,6,5,4,3,4,5,6,9,5,6,9,7,6,5,4,5,6,7,9,9,6,7,9,9,9,8,7,6,7,8,9,8,7,6,7,9,6,5,4,3,4,6,8,9,3,4,5,4,9,7,6,9,8,7,7,8,9,8,9,8,6,5,9,2,3,9,8,9,9,8,9,8,7,8,9,7,5,6,7,8]
   , [7,8,9,9,8,7,6,5,9,8,7,6,7,8,9,9,8,9,9,8,9,6,5,4,5,6,7,8,9,9,8,7,6,4,3,4,5,6,7,8,9,8,9,9,8,7,6,5,6,7,9,9,9,7,9,8,5,4,3,2,4,5,9,3,2,1,2,3,9,8,9,8,7,6,5,6,1,9,7,9,7,9,8,9,4,5,9,7,6,9,4,9,9,9,9,8,9,7,9,9]
   , [6,7,9,8,7,6,5,4,3,9,8,7,8,9,9,8,7,8,9,9,8,7,7,5,6,9,8,9,9,9,8,6,5,3,2,3,4,5,6,7,8,9,3,2,9,9,5,4,9,8,9,3,2,9,8,7,6,8,4,3,5,6,8,9,3,2,3,4,5,9,9,8,7,6,4,3,2,3,6,8,9,8,7,8,9,7,8,9,5,4,3,2,1,2,3,9,9,8,9,7]
   , [5,6,8,9,8,7,6,3,2,3,9,8,9,9,8,7,6,9,8,9,9,9,8,9,7,8,9,7,9,8,7,6,4,3,1,2,3,4,5,6,9,9,9,3,9,8,7,6,7,9,8,9,3,4,9,9,7,6,5,6,6,7,9,5,4,3,4,5,6,7,8,9,9,7,6,5,3,4,5,7,9,7,6,7,8,9,9,7,6,5,9,8,6,5,4,5,9,9,7,6]
   , [4,7,6,8,9,9,3,2,1,2,3,9,8,6,3,4,5,6,7,8,9,7,9,9,8,9,7,6,6,9,8,4,3,1,0,1,4,5,9,7,8,9,8,9,9,9,8,7,8,9,7,8,9,5,6,9,8,7,7,8,9,9,9,9,5,9,5,9,7,8,9,4,9,8,7,6,4,5,9,9,8,6,5,6,7,7,9,8,7,6,7,9,8,6,5,9,8,7,8,5]
   , [3,6,5,6,7,8,9,3,2,5,9,8,7,5,4,5,6,7,8,9,7,6,5,7,9,6,5,4,5,6,9,5,4,2,1,9,8,7,8,9,9,6,7,8,9,9,9,8,9,6,5,7,8,9,8,9,9,9,8,9,1,2,9,8,9,8,9,8,9,9,2,3,4,9,8,7,8,9,8,8,7,5,4,3,5,6,9,9,8,7,8,9,9,7,9,9,9,6,5,4]
   , [2,3,4,5,6,7,9,4,3,4,5,9,7,6,5,6,9,8,9,8,6,5,4,5,7,9,9,3,4,9,8,6,7,4,5,9,9,8,9,5,4,5,6,7,8,9,9,9,4,3,4,5,9,9,9,9,9,7,9,1,0,9,8,7,6,7,8,7,8,9,1,9,9,8,9,8,9,8,7,6,5,4,3,2,3,9,8,9,9,9,9,1,0,9,8,9,7,7,4,3]
   , [9,4,5,6,8,9,8,9,9,5,6,7,9,8,7,8,9,9,9,9,7,4,3,2,5,6,8,9,5,9,8,7,8,8,7,8,9,9,4,4,3,4,5,6,9,9,8,7,5,4,6,7,8,9,9,8,7,6,3,2,9,8,7,6,5,4,5,6,9,8,9,8,9,7,6,9,3,9,8,5,4,3,2,1,9,8,7,8,8,8,9,2,9,8,7,6,5,4,3,2]
   , [8,9,6,7,9,9,7,8,8,9,7,9,9,9,9,9,9,9,9,9,8,9,4,3,4,5,7,8,9,5,9,9,9,9,8,9,5,3,2,1,2,3,4,5,8,9,9,7,6,7,9,9,9,2,3,9,9,5,4,9,8,7,6,8,4,3,4,5,6,7,9,7,9,6,5,3,2,3,9,8,7,6,4,9,8,9,5,6,7,7,8,9,9,9,8,7,6,3,2,1]
   , [7,8,9,8,9,8,6,6,7,8,9,8,9,3,1,9,9,8,8,9,9,6,5,4,6,9,8,9,7,3,2,0,1,2,9,4,3,2,1,0,1,6,5,6,7,8,9,9,7,8,9,1,0,1,4,9,8,6,9,8,7,6,5,4,3,2,1,3,4,5,7,6,7,9,9,2,1,2,3,9,8,7,9,8,7,4,3,4,5,6,7,8,9,9,9,8,7,2,1,0]
   , [6,5,6,9,9,7,5,4,5,4,8,7,8,9,9,8,7,6,7,8,9,7,9,9,9,9,9,7,6,5,3,2,2,3,9,5,9,3,2,3,4,5,6,9,8,9,8,9,8,9,8,9,1,2,3,9,8,7,8,9,8,7,7,3,2,1,0,1,2,3,4,5,9,9,8,9,3,4,4,8,9,9,8,7,6,1,2,3,4,5,6,7,8,9,9,7,4,3,2,3]
   , [5,4,9,8,7,6,4,3,4,3,5,6,8,9,8,9,8,5,6,9,8,9,8,7,8,9,8,9,8,8,5,4,3,9,8,9,8,9,6,5,5,6,7,8,9,9,7,6,9,6,7,8,9,3,4,5,9,8,9,5,9,7,6,5,6,2,1,2,4,5,7,6,8,9,7,9,9,5,6,7,9,8,9,8,5,0,1,2,3,4,5,6,7,8,9,6,5,4,3,4]
   , [4,3,2,9,8,7,3,2,1,2,3,4,5,6,7,8,9,3,4,9,7,6,9,6,5,6,7,8,9,7,6,5,9,9,7,6,7,8,9,6,8,9,8,9,9,7,6,5,4,5,6,9,9,4,5,6,7,9,3,4,9,8,7,8,8,5,4,3,5,9,8,7,9,5,6,7,8,9,7,9,8,7,6,5,4,1,2,6,7,8,7,7,8,9,8,7,8,5,4,5]
   , [5,4,9,7,9,8,3,1,0,1,2,9,6,7,8,9,1,2,9,8,6,5,1,3,4,5,6,9,9,8,8,9,8,7,6,5,6,7,9,9,9,9,9,8,9,8,7,6,3,4,5,6,7,9,7,8,9,5,2,1,9,9,8,9,9,7,6,5,6,7,9,9,3,4,5,6,8,9,9,6,9,8,6,4,3,2,4,5,6,9,8,9,9,9,9,8,9,9,6,7]
   , [6,9,8,6,5,4,3,2,1,2,9,8,9,9,9,3,2,9,8,7,5,4,2,4,5,6,7,8,9,9,9,8,7,6,5,4,5,6,7,8,9,9,6,7,8,9,4,3,2,3,4,5,8,9,8,9,9,4,3,9,8,9,9,9,9,8,7,6,7,8,9,1,2,3,4,5,7,9,8,5,4,9,9,5,4,3,5,6,9,9,9,5,7,8,9,9,8,7,9,8]
   , [7,8,9,7,7,6,5,3,2,9,9,7,9,8,9,4,3,5,9,7,6,5,6,5,7,9,8,9,7,9,8,7,6,5,4,3,4,7,9,9,8,7,4,8,9,6,5,4,4,5,7,8,9,1,9,9,8,9,4,9,7,9,8,9,9,9,9,7,8,9,9,2,3,4,5,6,7,9,6,5,3,9,8,6,5,6,7,7,8,9,9,9,8,9,9,8,7,6,8,9]
   , [8,9,5,9,8,7,5,4,9,8,7,6,7,7,9,9,4,5,9,8,7,8,7,6,8,9,9,5,6,7,9,8,7,7,3,2,1,8,9,8,7,6,3,7,8,9,6,5,5,7,8,9,3,2,9,8,7,8,9,7,6,8,7,8,9,9,9,8,9,9,8,9,4,6,6,7,9,8,7,6,2,1,9,7,9,7,8,8,9,8,9,9,9,9,8,7,6,5,9,8]
   , [9,2,4,5,9,8,6,9,9,7,6,5,8,6,7,8,9,6,7,9,8,9,8,9,9,4,3,4,8,9,9,9,4,3,2,1,0,9,8,9,6,5,4,6,7,8,9,6,6,8,9,8,9,9,9,8,6,7,8,6,4,5,6,7,8,9,7,9,9,8,7,8,9,7,7,9,9,9,8,7,1,0,9,8,9,8,9,9,8,7,8,9,9,9,9,8,8,9,8,7]
   , [9,9,6,7,8,9,9,8,7,6,5,4,6,5,6,7,8,9,8,9,9,2,9,9,0,1,2,3,7,9,9,9,9,4,3,3,2,3,7,8,9,6,9,9,8,9,8,7,8,9,6,7,7,8,9,9,5,6,3,2,3,4,5,6,7,9,5,4,9,8,6,7,8,9,8,9,8,9,9,9,3,1,2,9,6,9,9,9,7,6,9,8,8,9,9,9,9,8,7,6]
   , [9,8,9,8,9,0,2,9,9,5,4,2,3,4,8,8,9,5,9,1,0,1,9,8,9,2,3,5,6,9,8,9,8,9,9,4,4,5,6,9,9,9,8,7,9,9,9,8,9,5,5,5,6,9,8,7,4,3,2,1,2,3,4,5,8,9,9,9,8,7,5,6,7,8,9,6,7,8,9,8,9,2,3,4,5,9,8,7,6,5,8,7,7,8,9,9,9,9,7,5]
   , [8,7,8,9,8,9,9,8,7,5,2,1,2,6,7,9,5,4,3,2,1,9,8,7,8,9,4,6,9,8,7,8,6,9,8,9,5,8,7,8,9,9,7,6,8,9,9,9,5,4,3,4,9,8,7,6,5,2,1,0,1,5,9,6,9,9,8,7,9,5,4,5,6,9,6,5,6,7,9,7,9,3,4,5,9,8,7,6,5,4,3,5,6,7,8,9,8,7,6,4]
   , [7,6,7,8,7,8,9,9,8,4,3,4,3,7,8,9,9,5,4,3,9,8,7,6,7,8,9,9,8,7,6,5,5,6,7,8,9,9,8,9,8,7,6,5,7,8,9,7,6,7,2,0,1,9,8,7,6,3,2,1,2,6,7,8,9,8,7,6,5,4,3,5,7,8,9,4,5,6,5,6,8,9,5,6,7,9,8,7,6,5,2,4,8,9,9,9,9,7,5,3]
   , [6,5,6,5,6,7,8,9,9,5,4,5,8,9,9,7,8,9,5,9,8,7,6,5,6,6,7,8,9,9,7,4,3,5,6,7,8,9,9,7,5,3,5,4,5,9,9,8,7,6,3,1,6,7,9,8,7,4,3,4,3,4,8,9,3,9,8,9,6,5,4,5,7,8,9,3,4,3,4,5,7,8,9,7,9,8,7,6,5,4,3,4,5,9,9,9,8,7,6,4]
   , [6,4,3,4,5,6,9,9,9,8,7,6,7,8,9,6,7,8,9,8,9,5,4,3,4,5,6,7,9,5,4,3,2,3,4,5,9,7,8,9,4,2,1,2,9,9,8,7,6,5,4,3,5,6,7,9,8,9,6,5,4,7,9,9,5,6,9,8,9,6,7,6,7,9,5,2,1,2,3,4,5,6,8,9,3,9,9,8,6,5,4,5,9,8,9,8,9,9,7,8]
   , [5,3,2,3,4,5,6,7,8,9,8,7,8,9,4,5,9,9,8,7,5,4,3,2,3,4,5,9,8,7,3,2,1,2,3,5,5,6,9,4,3,2,0,6,7,8,9,8,7,6,5,4,6,8,9,9,9,8,7,6,5,6,7,8,9,9,8,7,8,9,8,9,9,5,4,3,2,3,4,5,6,7,8,9,2,1,9,8,7,6,6,9,8,7,7,7,8,9,9,9]
   , [4,2,1,2,3,7,8,9,9,9,9,8,9,2,3,9,8,7,6,5,4,3,2,1,2,3,9,8,7,6,5,1,0,1,2,3,4,5,8,9,4,3,1,4,8,9,9,9,8,7,6,5,9,9,9,9,9,9,8,9,6,7,8,9,9,8,7,6,7,8,9,7,8,9,5,4,3,4,5,6,7,8,9,6,3,0,1,9,8,7,9,8,7,6,5,6,7,8,8,9]
   , [2,1,0,3,5,6,9,1,2,9,8,9,4,3,4,5,9,9,8,6,3,2,1,0,1,2,3,9,9,5,4,3,2,3,4,4,5,6,7,8,9,3,2,3,7,8,9,2,9,8,7,9,8,9,9,8,8,9,9,8,7,8,9,9,8,7,6,5,8,9,6,6,7,8,9,5,6,5,6,7,8,9,9,5,4,1,9,8,9,9,8,7,6,7,4,5,2,8,7,8]
   , [3,4,1,4,6,7,8,9,9,8,7,9,5,4,5,6,8,9,6,5,4,9,5,4,3,3,9,8,7,6,5,6,5,4,7,5,6,7,8,9,9,6,5,4,6,9,4,3,4,9,9,8,7,6,8,7,7,9,9,9,8,9,5,3,9,8,7,3,3,4,5,5,6,9,9,6,7,7,8,9,9,9,8,6,5,9,8,7,8,6,9,6,5,4,3,2,1,2,6,7]
   , [4,9,2,5,7,8,9,9,8,7,6,8,9,9,8,7,9,8,7,6,9,8,9,9,4,9,9,9,8,7,8,9,6,5,6,7,8,9,9,9,8,7,6,5,8,9,5,4,6,9,8,7,6,5,4,6,6,7,8,9,9,9,3,2,9,9,3,2,1,2,3,4,6,8,8,9,8,9,9,4,3,2,9,7,8,9,8,6,7,5,9,7,6,9,8,3,4,3,4,6]
   , [9,8,9,6,7,8,9,7,6,4,5,6,7,8,9,8,9,9,8,9,9,7,7,8,9,8,9,9,9,8,9,8,7,6,7,8,9,8,9,7,9,8,7,6,7,8,9,5,9,8,9,6,5,4,3,5,5,8,9,9,9,8,9,9,8,5,4,3,0,1,2,3,5,6,7,8,9,8,9,5,4,3,9,8,9,9,7,5,3,4,5,9,9,8,7,6,5,7,5,6]
   , [6,7,8,9,8,9,7,6,4,3,4,7,6,7,9,9,1,0,9,9,7,6,6,7,8,7,8,8,9,9,7,9,8,7,8,9,6,7,9,6,5,9,9,7,8,9,9,9,8,7,6,5,4,3,2,4,4,5,8,9,8,7,9,8,7,6,5,4,7,2,3,4,5,7,8,9,6,7,8,9,5,9,8,9,9,8,9,4,2,3,4,5,9,9,8,7,9,8,7,7]
   , [5,6,7,8,9,7,5,4,3,2,1,2,5,6,8,9,3,9,8,7,6,5,5,6,7,6,7,7,8,9,6,4,9,8,9,6,5,9,8,9,4,3,4,9,9,9,9,8,7,6,5,4,3,2,1,2,3,4,7,8,9,6,7,9,8,7,6,5,6,7,8,8,7,8,9,6,5,6,7,8,9,9,7,9,8,7,5,3,1,4,5,6,8,9,9,8,9,9,8,8]
   , [4,5,6,7,9,9,6,6,4,5,0,5,4,5,7,8,9,8,7,6,5,4,3,2,3,4,5,6,9,5,4,3,2,9,2,5,4,5,7,8,9,2,3,4,5,9,8,7,6,5,4,3,2,1,0,1,2,3,6,8,9,5,4,3,9,8,7,8,7,9,9,9,8,9,8,7,3,7,8,9,6,5,6,9,9,4,3,2,0,1,5,6,7,8,9,9,8,9,9,9]
   , [3,4,5,6,7,8,9,7,5,2,1,2,3,5,6,7,8,9,8,7,6,5,1,0,4,5,6,9,5,4,3,2,1,0,1,2,3,4,6,9,2,1,2,3,6,9,9,8,7,6,5,6,5,2,1,2,3,4,5,9,7,6,5,2,3,9,8,9,8,9,8,7,9,9,9,8,5,6,7,8,9,4,9,8,7,6,5,3,1,2,3,7,8,9,5,6,7,7,8,9]
   , [2,1,2,7,8,9,7,6,5,4,3,4,5,8,7,8,9,4,9,8,7,4,3,2,5,6,7,8,9,9,8,3,2,3,3,4,5,5,7,8,9,0,3,5,7,8,9,9,8,8,6,7,4,3,2,3,4,5,6,8,9,3,2,1,2,3,9,9,9,8,9,6,9,9,9,9,6,7,8,9,4,3,4,9,9,8,6,8,9,4,4,6,9,5,4,6,5,6,9,8]
   , [4,5,7,8,9,9,8,7,6,5,4,5,6,7,8,9,2,3,4,9,6,5,4,5,6,7,8,9,9,7,6,5,3,4,5,5,9,6,7,9,2,1,4,6,7,8,9,5,9,9,9,8,5,5,3,4,5,6,7,8,9,8,7,5,3,4,7,8,9,7,6,5,7,8,9,8,7,8,9,4,5,2,3,4,9,9,7,9,8,5,5,8,9,4,3,2,4,5,6,7]
   , [5,6,8,9,4,3,9,8,7,7,6,7,7,8,9,0,1,2,5,9,8,7,7,6,7,8,9,4,9,8,7,8,6,5,6,9,8,7,8,9,3,2,5,8,8,9,5,4,6,7,9,9,7,6,7,8,6,8,8,9,5,9,8,7,6,5,6,7,8,9,5,4,5,6,8,9,8,9,4,3,2,1,3,4,8,9,9,8,7,6,6,8,9,3,2,1,2,3,9,8]
   , [6,7,8,9,5,2,1,9,9,8,8,8,9,9,2,1,2,3,6,7,9,9,8,7,8,9,2,3,5,9,8,9,7,8,9,9,9,8,9,5,4,5,6,7,8,9,6,3,4,5,6,9,8,7,8,9,7,8,9,3,4,5,9,9,8,6,7,9,9,6,5,3,4,8,9,9,9,4,3,2,1,0,7,6,7,9,9,9,9,8,7,9,6,4,3,2,3,5,6,9] ]