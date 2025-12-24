module AdventOfCode.Y2025.Day02 where

import Data.List (nub)

type Range = (Int,Int)

sampleInput :: [Range]
sampleInput = [(11,22),(95,115),(998,1012),(1188511880,1188511890),(222220,222224),(1698522,1698528),(446443,446449),(38593856,38593862),(565653,565659),(824824821,824824827),(2121212118,2121212124)]

myInput :: [Range]
myInput = [(1090286,1131879),(3259566,3404881),(138124,175118),(266204727,266361099),(16765,24272),(7657360692,7657593676),(88857504,88926597),(6869078,6903096),(48444999,48532270),(61427792,61580535),(71,103),(8077,10421),(1920,2560),(2,17),(951,1259),(34,50),(28994,36978),(1309,1822),(9393918461,9393960770),(89479,120899),(834641,988077),(5389718924,5389797353),(34010076,34214499),(5063,7100),(607034,753348),(19098586,19261191),(125085556,125188689),(39839,51927),(3246,5037),(174,260),(439715,473176),(187287,262190),(348,535),(58956,78301),(4388160,4505757),(512092,584994),(13388753,13534387)]

part1 :: [Range] -> Int
part1 ranges = sum (ranges >>= getInvalidIds1)

part2 :: [Range] -> Int
part2 ranges = sum $ nub (ranges >>= getInvalidIds2)

getInvalidIds1 :: Range -> [Int]
getInvalidIds1 (a,b) = let
  la = length $ show a
  lb = length $ show b
  c = 10 ^ la
  -- split cases if a b are different number of digits
  in if la /= lb then getInvalidIds1 (a,c-1) ++ getInvalidIds1 (c,b) else let
  (d,m) = divMod la 2
  r = 10 ^ d + 1
  in if m == 1 then [] else filter (\x -> x `mod` r == 0) [a..b]


getInvalidIds2 :: Range -> [Int]
getInvalidIds2 (a,b) = let
  la = length $ show a
  lb = length $ show b
  c = 10 ^ la
  -- split cases if a b are different number of digits
  in if la /= lb then getInvalidIds2 (a,c-1) ++ getInvalidIds2 (c,b) else [2..la] >>= \reps -> let
    (d,m) = divMod la reps
    r = sum $ map (\rep -> 10 ^ (rep * d)) [0..reps-1]
    in if m /= 0 then [] else filter (\x -> x `mod` r == 0) [a..b]

{-
la = 6
reps = 2
d = 3
123123
r=1001
0 -> 1    = 10^0
1 -> 1000 = 10^d

la = 12
reps = 3
d = la `mod` reps = 4
775477547754
r= 100010001

0 -> 1         = 10^0
1 -> 10000     = 10^d
2 -> 100000000 = 10^(d*2)
-}





