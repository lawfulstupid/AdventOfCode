module AdventOfCode.Y2021.Day15 where

import AdventOfCode.Common.Grid
import AdventOfCode.Common.List
import AdventOfCode.Common.Tuple

import Data.Maybe
import Data.List
import Data.Ord

--------------------------------------------------------------------------------

type RiskLevel = Int
type Path = [Coords]

--------------------------------------------------------------------------------

minRiskLevelPath :: Grid Int -> Maybe Path
minRiskLevelPath g = aux [(0,0)] where
   
   target = (subtract 1) $# dimensions g
   
   aux :: Path -> Maybe Path
   aux path | last path == target = Just path
   aux path = let
      source = last path
      score p = totalRisk g (path ++ [p]) + heuristic g p target
      candidates = safeHead []
         $ groupOn score
         $ sortOn score
         $ filter (not . (`elem` path))
         $ coordNeighbours g source
      nextStep p = aux (path ++ [p])
      solns = catMaybes $ map nextStep candidates
      in if solns == []
         then Nothing
         else Just $ minimumBy (comparing $ totalRisk g) solns
   
   getCandidates :: [[(Coords, RiskLevel)]] -> [(Coords, RiskLevel)]
   getCandidates [] = []
   getCandidates (x:xs) = let
      threshold = snd (head x) + 0
      in concat $ filter (\y -> snd (head y) <= threshold) (x:xs)
      
heuristic :: Grid RiskLevel -> Coords -> Coords -> RiskLevel
-- heuristic g (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
heuristic g p _ = let
   g' = drop2d p g
   rowMins = tail $ mapRows minimum g'
   colMins = tail $ mapCols minimum g'
   in sum (rowMins ++ colMins)

totalRisk :: Grid RiskLevel -> Path -> RiskLevel
totalRisk g path = sum $ map (g#) $ tail path

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x:xs) = x

drawPath :: Grid RiskLevel -> IO ()
drawPath g = let
   path = fromJust $ minRiskLevelPath g
   highlight r = "\ESC[31m" ++ show r ++ "\ESC[0m"
   g' = mapWithCoords (\p r -> if p `elem` path then highlight r else show r) g
   in putStrLn $ showPretty '\NUL' "" id g'

--------------------------------------------------------------------------------

test = take 40 <$> minRiskLevelPath sampleInput

--------------------------------------------------------------------------------

sampleInput :: Grid RiskLevel
sampleInput = Grid
   [ [1,1,6,3,7,5,1,7,4,2]
   , [1,3,8,1,3,7,3,6,7,2]
   , [2,1,3,6,5,1,1,3,2,8]
   , [3,6,9,4,9,3,1,5,6,9]
   , [7,4,6,3,4,1,7,1,1,1]
   , [1,3,1,9,1,2,8,1,3,7]
   , [1,3,5,9,9,1,2,4,2,1]
   , [3,1,2,5,4,2,1,6,3,9]
   , [1,2,9,3,1,3,8,5,2,1]
   , [2,3,1,1,9,4,4,5,8,1] ]

-- aux (1,0) (9,9) 1 = let
   -- f p = 1 + sampleInput # p
   -- candidates = [(2,0),(0,0),(1,1)]
   -- next = minimumBy (comparing $ \p -> 1 + (sampleInput # p) + h p t) [(2,0),(0,0),(1,1)]
   -- in s : aux next t (f next)
