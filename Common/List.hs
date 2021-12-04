module AdventOfCode.Common.List where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print

(!?) :: [a] -> Int -> Maybe a
(!?) xs n = if 0 <= n && n < length xs then Just (xs !! n) else Nothing

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

padL :: a -> Int -> [a] -> [a]
padL x n xs = replicate (n - length xs) x ++ xs

deleteAt :: Int -> [a] -> [a]
deleteAt n xs = let
   (h,_:t) = splitAt n xs
   in h ++ t

deleteAllAt :: [Int] -> [a] -> [a]
deleteAllAt ns xs = concat $ zipWith (\n x -> if n `elem` ns then [] else [x]) [0..] xs
