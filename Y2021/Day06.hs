module AdventOfCode.Y2021.Day6 where

import AdventOfCode.Common.List (printLines)

--------------------------------------------------------------------------------

type Fish = Int
type Time = Int

--------------------------------------------------------------------------------

-- How many descendants (incl. self) does a fish have given its current age
-- and the number of steps forward to look

descendants :: Time -> Fish -> Int
descendants t f = descendantsMem !! t !! f

descendantsMem :: [[Int]]
descendantsMem = [[ aux t f | f <- [0..8]] | t <- [0..]] where
   aux :: Time -> Fish -> Int
   aux 0 f = 1
   aux t 0 = descendants (t-1) 6 + descendants (t-1) 8
   aux t f = descendants (t-1) (f-1)

update :: Fish -> [Fish]
update 0 = [6,8]
update n = [n-1]

updateAll :: [Fish] -> [Fish]
updateAll fish = fish >>= update

--------------------------------------------------------------------------------

part1 :: [Fish] -> Int
part1 = sum . map (descendants 80)

--------------------------------------------------------------------------------

sampleInput :: [Fish]
sampleInput = [3,4,3,1,2]

myInput :: [Fish]
myInput = [1,1,1,3,3,2,1,1,1,1,1,4,4,1,4,1,4,1,1,4,1,1,1,3,3,2,3,1,2,1,1,1,1,1,1,1,3,4,1,1,4,3,1,2,3,1,1,1,5,2,1,1,1,1,2,1,2,5,2,2,1,1,1,3,1,1,1,4,1,1,1,1,1,3,3,2,1,1,3,1,4,1,2,1,5,1,4,2,1,1,5,1,1,1,1,4,3,1,3,2,1,4,1,1,2,1,4,4,5,1,3,1,1,1,1,2,1,4,4,1,1,1,3,1,5,1,1,1,1,1,3,2,5,1,5,4,1,4,1,3,5,1,2,5,4,3,3,2,4,1,5,1,1,2,4,1,1,1,1,2,4,1,2,5,1,4,1,4,2,5,4,1,1,2,2,4,1,5,1,4,3,3,2,3,1,2,3,1,4,1,1,1,3,5,1,1,1,3,5,1,1,4,1,4,4,1,3,1,1,1,2,3,3,2,5,1,2,1,1,2,2,1,3,4,1,3,5,1,3,4,3,5,1,1,5,1,3,3,2,1,5,1,1,3,1,1,3,1,2,1,3,2,5,1,3,1,1,3,5,1,1,1,1,2,1,2,4,4,4,2,2,3,1,5,1,2,1,3,3,3,4,1,1,5,1,3,2,4,1,5,5,1,4,4,1,4,4,1,1,2]
