module AdventOfCode.Y2020.Common (
   module AdventOfCode.Y2020.Common,
   module AdventOfCode.Common.Util,
   module AdventOfCode.Common.List,
   module AdventOfCode.Common.Parser
) where

import AdventOfCode.Common.Util
import AdventOfCode.Common.List
import AdventOfCode.Common.Parser

import Data.Tuple (swap)
import Data.Map (Map)
import qualified Data.Map as M

solveToBijection :: (Ord a, Ord b) => Map a [b] -> Map a b
solveToBijection = aux M.empty
   where
   aux :: (Ord a, Ord b) => Map a b -> Map a [b] -> Map a b
   aux out m | M.size m == 0 = out
   aux out m = let
      (final, ambig) = M.partition (\xs -> length xs == 1) m
      confirmedAssocs = M.assocs $ M.map head final
      out' = M.union out $ M.fromList confirmedAssocs
      confirmed = map snd confirmedAssocs
      m' = M.map (filter (\x -> not (x `elem` confirmed))) ambig
      in aux out' m'
