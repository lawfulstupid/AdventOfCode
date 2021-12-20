{-# LANGUAGE TupleSections #-}

module AdventOfCode.Common.List where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)
import Data.List

(!?) :: [a] -> Int -> Maybe a
(!?) xs n = if 0 <= n && n < length xs then Just (xs !! n) else Nothing

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

freq :: Ord a => [a] -> [(a, Int)]
freq xs = map (\g -> (g !! 0, length g)) $ group $ sort xs

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

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

thenBy :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
thenBy c1 c2 a b = let r1 = c1 a b in if r1 /= EQ then r1 else c2 a b

reverseMap :: (Ord a, Ord b) => Map a [b] -> Map b [a]
reverseMap m = grouping $ map swap (M.assocs m >>= \(k,vs) -> map (k,) vs)

padL :: a -> Int -> [a] -> [a]
padL x n xs = replicate (n - length xs) x ++ xs

substringIndex :: Eq a => [a] -> [a] -> Maybe Int
substringIndex sub [] = Nothing
substringIndex sub str = if isPrefixOf sub str then Just 0 else (+1) <$> substringIndex sub (tail str)

deleteAt :: Int -> [a] -> [a]
deleteAt n xs = let
   (h,_:t) = splitAt n xs
   in h ++ t

deleteAllAt :: [Int] -> [a] -> [a]
deleteAllAt ns xs = concat $ zipWith (\n x -> if n `elem` ns then [] else [x]) [0..] xs

expBinSearchOn :: (Ord b) => (a -> b) -> b -> [a] -> Maybe a
expBinSearchOn f t [] = Nothing
expBinSearchOn f t (x:xs) = expSearch ([x],xs) where
   
   expSearch (lo,hi)
      | null hi || t < f (head hi) = binSearch lo
      | otherwise = expSearch $ splitAt (2 * length lo) hi
   
   binSearch [] = Nothing
   binSearch xs = let
      m = length xs `div` 2
      p = xs !! m
      in case compare t (f p) of
         LT -> binSearch $ take m xs
         EQ -> Just p
         GT -> binSearch $ drop (m+1) xs
