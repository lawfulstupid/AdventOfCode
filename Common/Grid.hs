module AdventOfCode.Common.Grid where

import qualified Data.List as L
import AdventOfCode.Common.List ((!?), padL)

newtype Grid a = Grid { unpack :: [[a]] }
   deriving (Eq)

instance Show a => Show (Grid a) where
   show g = let
      colLengths = mapCols maximum $ fmap (length . show) g
      showCell n cell = padL ' ' n $ show cell
      showRow row = L.intercalate " " $ zipWith showCell colLengths row
      in L.intercalate "\n" $ mapRows showRow g

instance Functor Grid where
   fmap f (Grid g) = Grid $ map (map f) g

transpose :: Grid a -> Grid a
transpose (Grid g) = Grid $ L.transpose g

mapRows :: ([a] -> b) -> Grid a -> [b]
mapRows f (Grid g) = map f g

mapCols :: ([a] -> b) -> Grid a -> [b]
mapCols f = mapRows f . transpose

filterRows :: ([a] -> Bool) -> Grid a -> Grid a
filterRows f (Grid g) = Grid $ filter f g

filterCols :: ([a] -> Bool) -> Grid a -> Grid a
filterCols f = transpose . filterRows f . transpose

height :: Grid a -> Int
height (Grid g) = length g

width :: Grid a -> Int
width (Grid g) = if length g == 0 then 0 else length (g !! 0)

row :: Int -> Grid a -> Maybe [a]
row n (Grid g) = g !? n

col :: Int -> Grid a -> Maybe [a]
col n = row n . transpose

(!) :: Grid a -> (Int, Int) -> a
(!) (Grid g) (x,y) = (g !! y) !! x

fromCoordsList :: a -> [((Int, Int), a)] -> Grid a
fromCoordsList def coords = let
   maxX = maximum $ map (fst . fst) coords
   maxY = maximum $ map (snd . fst) coords
   getValue (x,y) = maybe def id $ lookup (x,y) coords
   in Grid [ [ getValue (x,y) | x <- [0..maxX]] | y <- [0..maxY]]