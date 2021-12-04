module AdventOfCode.Common.List where

import Data.Map (Map)
import qualified Data.Map as M

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p [] = [[]]
breakAll p (x:xs) = let
   (a:b) = breakAll p xs
   in if p x then []:(a:b) else (x:a):b

splitAll :: Int -> [a] -> [[a]]
splitAll n [] = []
splitAll n xs = let (a,b) = splitAt n xs in a : splitAll n b

grouping :: Ord k => [(k,a)] -> Map k [a]
grouping = M.fromListWith (++) . map (fmap return)

reverseMap :: (Ord a, Ord b) => Map a [b] -> Map b [a]
reverseMap m = grouping $ map swap (M.assocs m >>= \(k,vs) -> map (k,) vs)
