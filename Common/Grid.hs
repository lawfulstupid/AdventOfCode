module AdventOfCode.Common.Grid where

import qualified Data.List as L
import Data.Maybe (fromJust, catMaybes, listToMaybe, isJust)
import AdventOfCode.Common.List ((!?), padL)

newtype Grid a = Grid { unpack :: [[a]] }
   deriving (Eq)

type Coords = (Int, Int)

instance Show a => Show (Grid a) where
   show = showPretty ' ' " " show

showPretty :: Char -> String -> (a -> String) -> Grid a -> String
showPretty pad delim show g = let
   colLengths = mapCols maximum $ fmap (length . show) g
   showCell n cell = padL pad n $ show cell
   showRow row = L.intercalate delim $ zipWith showCell colLengths row
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

dimensions :: Grid a -> Coords
dimensions g = (width g, height g)

row :: Int -> Grid a -> Maybe [a]
row n (Grid g) = g !? n

col :: Int -> Grid a -> Maybe [a]
col n = row n . transpose

(#) :: Grid a -> Coords -> a
(#) g p = fromJust (g #? p)

(#?) :: Grid a -> Coords -> Maybe a
(#?) (Grid g) (x,y) = (g !? y) >>= (!? x)

set :: Coords -> a -> Grid a -> Grid a
set p x = mapWithCoords $ \p' -> if p == p' then const x else id

setAll :: [(Coords, a)] -> Grid a -> Grid a
setAll pxs g = foldr (uncurry set) g pxs

modify :: Coords -> (a -> a) -> Grid a -> Grid a
modify p f g = set p (f (g # p)) g

toCoordsList :: Grid a -> [(Coords, a)]
toCoordsList g = concat $ unpack $ mapWithCoords (,) g

fromCoordsList :: a -> [(Coords, a)] -> Grid a
fromCoordsList def coords = let
   maxX = maximum $ map (fst . fst) coords
   maxY = maximum $ map (snd . fst) coords
   getValue (x,y) = maybe def id $ lookup (x,y) coords
   in Grid [ [ getValue (x,y) | x <- [0..maxX]] | y <- [0..maxY]]

fromList :: Int -> [a] -> Grid a
fromList width values
   | length values `mod` width /= 0 = errorWithoutStackTrace "Grid width does not divide data length"
   | otherwise = Grid $ aux values
   where
   aux :: [a] -> [[a]]
   aux [] = []
   aux xs = let (a,b) = splitAt width xs in a : aux b

coordGrid :: (Int, Int) -> Grid Coords
coordGrid (w,h) = Grid [[(x-1,y-1) | x <- [1..w]] | y <- [1..h]]

coordNeighbours :: Coords -> [Coords]
coordNeighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

neighbours :: Grid a -> Coords -> [a]
neighbours g (x,y) = catMaybes $ map (g #?) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

neighboursD :: Grid a -> Coords -> [a]
neighboursD g (x,y) = catMaybes $ map (g #?) [(x+i, y+j) | i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0]

mapWithCoords :: (Coords -> a -> b) -> Grid a -> Grid b
mapWithCoords f g = fmap (\p -> f p (g # p)) $ coordGrid $ dimensions g

findCoords :: (a -> Bool) -> Grid a -> Maybe Coords
findCoords f g = listToMaybe $ catMaybes $ concat $ unpack $ mapWithCoords (\p x -> if f x then Just p else Nothing) g

drop2d :: Coords -> Grid a -> Grid a
drop2d (x,y) (Grid g) = Grid (map (drop x) $ drop y g)

join :: Grid (Grid a) -> Grid a
join g = foldr1 joinH $ mapCols (foldr1 joinV) g

joinH :: Grid a -> Grid a -> Grid a
joinH (Grid g1) (Grid g2) = Grid $ zipWith (++) g1 g2

joinV :: Grid a -> Grid a -> Grid a
joinV (Grid g1) (Grid g2) = Grid (g1 ++ g2)
