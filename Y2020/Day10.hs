{-# LANGUAGE BangPatterns #-}

module AdventOfCode.Y2020.Day10 where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import GHC.IO

type Joltage = Integer
type Adapter = Integer
type Bag = [Adapter]
type AdapterChain = [Adapter]
type AdapterList = [Adapter]
type Counter a = Map a Int

deviceAdapter :: Bag -> Adapter
deviceAdapter bag = maximum bag + 3

getCount :: Ord a => a -> Counter a -> Int
getCount x m = fromMaybe 0 $ Map.lookup x m


joltDiffs :: Bag -> Counter Joltage
joltDiffs bag = aux Map.empty $ sort (0 : deviceAdapter bag : bag) where
   aux :: Counter Joltage -> AdapterChain -> Counter Joltage
   aux m (x:y:list) = let
      diff = y - x
      count = getCount diff m
      in aux (Map.insert diff (count+1) m) (y:list)
   aux m _ = m

part1 :: Bag -> Int
part1 bag = let
   m = joltDiffs bag
   diff1 = getCount 1 m
   diff3 = getCount 3 m
   in diff1 * diff3


   
accepts :: Joltage -> Adapter -> Bool
accepts input adapter = adapter - 3 <= input && input < adapter

nextAdapter :: Joltage -> Bag -> [Adapter]
nextAdapter input bag = filter (accepts input) bag

arrangementsBounded :: Joltage -> AdapterList -> Joltage -> Int
arrangementsBounded input [] target = if target - 3 <= input then 1 else 0
arrangementsBounded input (adapter:adapters) target
   | input < adapter - 3 = 0
   | otherwise = let
      !next = arrangementsBounded input adapters target
      this = if input < adapter then arrangementsBounded adapter adapters target else 0
      in this + next

solveSplit :: AdapterList -> Int
solveSplit adapters | length adapters <= 1 = 1
solveSplit adapters = let
   input = head adapters
   output = last adapters
   middle = init $ tail adapters
   in arrangementsBounded input middle output
   
splitAtDiff3 :: [Adapter] -> [[Adapter]]
splitAtDiff3 [] = [[]]
splitAtDiff3 [x] = [[x]]
splitAtDiff3 (x:y:xs) = if y - x >= 3
   then [[x]] ++ splitAtDiff3 (y:xs)
   else let
      (a:b) = splitAtDiff3 (y:xs)
      in (x:a):b

part2 :: Bag -> Int
part2 bag = let
   target = deviceAdapter bag
   adapters = sort (0:target:bag)
   splits = splitAtDiff3 adapters
   in product $ map solveSplit splits


-- This module was compiled to squeeze performance
main :: IO ()
main = print (part2 myInput)


sample :: Bag
sample = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]

sample2 :: Bag
sample2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

myInput :: Bag
myInput = [153, 69, 163, 123, 89, 4, 135, 9, 124, 74, 141, 132, 75, 3, 18, 134, 84, 15, 61, 91,
   90, 98, 99, 51, 131, 166, 127, 77, 106, 50, 22, 70, 43, 28, 41, 160, 44, 117, 66, 60, 76, 17,
   138, 105, 97, 161, 116, 49, 104, 169, 71, 100, 16, 54, 168, 42, 57, 103, 1, 32, 110, 48, 12,
   143, 112, 82, 25, 81, 148, 133, 144, 118, 80, 63, 156, 88, 47, 115, 36, 2, 94, 128, 35, 62,
   109, 29, 40, 19, 37, 122, 142, 167, 7, 147, 121, 159, 87, 83, 111, 162, 150, 8, 149]
