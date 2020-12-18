{-# LANGUAGE PartialTypeSignatures #-}

module AdventOfCode.Y2020.Day13 where

import AdventOfCode.Y2020.Common
import Data.List
import Data.Maybe

type Timestamp = Int
data Id = Id Int | X
   deriving (Show, Eq)


data Input = Input Timestamp [Id]
    deriving (Show,Eq)

parse :: String -> [Id]
parse = aux . breakAll (==',') where
    aux [] = []
    aux ("x":xs) = X : aux xs
    aux (x:xs) = Id (read x) : aux xs

-- returns the least multiple of n >= t
nextMultiple :: Int -> Timestamp -> Timestamp
nextMultiple n t = let
   (d,m) = t `divMod` n
   k = if m == 0 then d else d+1
   in k * n

part1 :: Input -> Int
part1 (Input t ids) = let
   (id, d) = head $ sortOn snd [(n, nextMultiple n t) | Id n <- ids]
   in id * (d-t)


type Pattern = (Int, Int)

equivIn :: Int -> Int -> Int -> Bool
equivIn n a b = a `mod` n == b `mod` n

match :: Pattern -> Int -> Bool
match (n,r) x = equivIn n x r

generate :: Pattern -> [Int]
generate (n,r) = map (\k -> n * k + r) [0..]

both :: Pattern -> Pattern -> Pattern
both p1@(n1,r1) p2@(n2,r2)
   | n1 < n2 = both p2 p1
   | otherwise = let
      n = lcm n1 n2
      r = head $ filter (match p2) $ generate p1
      in (n,r)

part2 :: Input -> Int
part2 (Input _ ids) = let
   patterns = [(n, -i) | (i, Id n) <- zip [0..] ids]
   in snd $ foldr1 both patterns
   

sample :: [Input]
sample = 
   [ Input 939 $ parse "7,13,x,x,59,x,31,19"
   , Input undefined $ parse "17,x,13,19"
   , Input undefined $ parse "67,7,59,61"
   , Input undefined $ parse "67,x,7,59,61"
   , Input undefined $ parse "67,7,x,59,61"
   , Input undefined $ parse "1789,37,47,1889" ]

myInput :: Input
myInput = Input 1004098 $ parse "23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,509,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,401,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,19"
