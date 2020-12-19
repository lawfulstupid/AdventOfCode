{-# LANGUAGE BangPatterns #-}

module AdventOfCode.Y2020.Day15 where

import Data.List
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

import Control.DeepSeq

type Term = Int
type Sequence = [Term]

-- Record: 85.48s
getTerm :: Int -> Sequence -> Term
getTerm n seed | n <= 0 = errorWithoutStackTrace "index out of bounds"
getTerm n seed | n <= length seed = seed !! (n-1)
getTerm n seed = let
   !lastIndexMap = M.fromList $ zip (init seed) $ iterate pred (n-1)
   in aux (n - length seed) (last seed) lastIndexMap
   where
   aux :: Int -> Term -> IntMap Int -> Term
   aux 0 term _ = term
   aux idx term lastIndexMap = let
      !nextTerm = M.findWithDefault idx term lastIndexMap - idx
      !lastIndexMap' = M.insert term idx lastIndexMap
      in aux (idx-1) nextTerm lastIndexMap'

part1 = getTerm 2020
part2 = getTerm 30000000

sample1 = [0,3,6] :: Sequence
sample2 = [1,3,2] :: Sequence
sample3 = [2,1,3] :: Sequence
sample4 = [1,2,3] :: Sequence
sample5 = [2,3,1] :: Sequence
sample6 = [3,2,1] :: Sequence
sample7 = [3,1,2] :: Sequence
myInput = [2,15,0,9,1,20] :: Sequence
