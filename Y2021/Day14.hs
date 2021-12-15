module AdventOfCode.Y2021.Day14 where

import AdventOfCode.Common.Parser
import AdventOfCode.Common.List
import AdventOfCode.Common.Util
import Data.List
import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------

type Element = Char
type Polymer = String

data Rule = Rule
   { input :: SimplePolymer
   , inject :: Element
   } deriving (Eq)

data PolymerTemplate = PolymerTemplate
   { polymer :: Polymer
   , rules :: [Rule] }

-- A polymer of just two elements, the smallest a polymer can be
type SimplePolymer = (Element, Element)

-- Represents the number of elements in a polymer
type ElementCount = Map Element Int

-- A sequence showing growth of a polymer over time as its element counts
type GrowthSequence = [ElementCount]

-- A map describing growth of simple polymers
type PolymerMem = Map SimplePolymer GrowthSequence

--------------------------------------------------------------------------------

-- Combines two element counts by summing their counts for respective elements
(<+>) :: ElementCount -> ElementCount -> ElementCount
(<+>) = M.unionWith (+)

-- Constructs the polymer output of a rule
output :: Rule -> Polymer
output (Rule (l,r) i) = [l,i,r]

-- Decreases a natural number, does not allow negatives
decrement :: Int -> Maybe Int
decrement x | x <= 0 = Nothing
decrement x = Just (x-1)

-- Deconstructs a polymer into a list of pairs
pairs :: Polymer -> [SimplePolymer]
pairs [_] = []
pairs (x:y:xs) = (x,y) : pairs (y:xs)

-- Given a set of rules, constructs a memoized model of simple polymer growth
getPolymerMem :: [Rule] -> PolymerMem
getPolymerMem rules = mem where
   mem = M.fromList $ map aux rules
   
   -- Expands a rule into a growth sequence
   aux :: Rule -> (SimplePolymer, GrowthSequence)
   aux rule = let
      h = simpleCount (input rule)              -- the inital element count
      t = getGrowthSequence mem (output rule)   -- using the above memoized map, construct the rest of the growth sequence from the rule output
      in (input rule, h:t)

   simpleCount :: SimplePolymer -> ElementCount
   simpleCount (a,b) = if a == b
      then M.fromDistinctAscList [(a,2)]
      else M.fromList [(a,1),(b,1)]

-- Computes the growth sequence of a polymer, given 
getGrowthSequence :: PolymerMem -> Polymer -> GrowthSequence
getGrowthSequence mem p = let
   -- Gets "Local" Growth Sequence of a simple polmer from the map
   -- This ignore the last character of the grown polymer
   -- We do this to avoid counting twice when recombining
   getLocalGS (a,b) = map (M.update decrement b) $ fromJust $ M.lookup (a,b) mem
   -- Compute the local growth of each simple polymer in our input
   polymerGrowth = map getLocalGS $ pairs p :: [GrowthSequence]
   -- Add the last element back in
   lastElementGrowth = repeat $ M.fromList [(last p, 1)] :: GrowthSequence
   -- This contains all our growth subsequences in parallel
   totalGrowth = polymerGrowth ++ [lastElementGrowth] :: [GrowthSequence]
   -- Swap rows and columns so we can sum across all counts by time step in the growth
   in map (foldr1 (<+>)) $ transpose totalGrowth

-- What we want to ultimately compute
getElementCount :: PolymerMem -> Polymer -> Int -> ElementCount
getElementCount mem p n = getGrowthSequence mem p !! n

differential :: ElementCount -> Int
differential ec = let freqs = M.elems ec in maximum freqs - minimum freqs

part1 :: PolymerTemplate -> Int
part1 pt = differential $ getElementCount (getPolymerMem $ rules pt) (polymer pt) 10

part2 :: PolymerTemplate -> Int
part2 pt = differential $ getElementCount (getPolymerMem $ rules pt) (polymer pt) 40

--------------------------------------------------------------------------------

ruleParser :: Parser Rule
ruleParser = do
   l <- char
   r <- char
   match " -> "
   i <- char
   return $ Rule (l,r) i

makeTemplate :: String -> [String] -> PolymerTemplate
makeTemplate p rs = PolymerTemplate p $ map (parseUsing ruleParser) rs

sampleInput :: PolymerTemplate
sampleInput = makeTemplate "NNCB" ["CH -> B","HH -> N","CB -> H","NH -> C","HB -> C","HC -> B","HN -> C","NN -> C","BH -> H","NC -> B","NB -> B","BN -> B","BB -> N","BC -> B","CC -> N","CN -> C"]

myInput :: PolymerTemplate
myInput = makeTemplate "ONHOOSCKBSVHBNKFKSBK" ["HO -> B","KB -> O","PV -> B","BV -> C","HK -> N","FK -> H","NV -> C","PF -> K","FV -> B","NH -> P","CO -> N","HV -> P","OH -> H","BC -> H","SP -> C","OK -> F","KH -> N","HB -> V","FP -> N","KP -> O","FB -> O","FH -> F","CN -> K","BP -> P","SF -> O","CK -> K","KN -> O","VK -> C","HP -> N","KK -> V","KO -> C","OO -> P","BH -> B","OC -> O","HC -> V","HS -> O","SH -> V","SO -> C","FS -> N","CH -> O","PC -> O","FC -> S","VO -> H","NS -> H","PH -> C","SS -> F","BN -> B","BF -> F","NC -> F","CS -> F","NN -> O","FF -> P","OF -> H","NF -> O","SC -> F","KC -> F","CP -> H","CF -> K","BS -> S","HN -> K","CB -> P","PB -> V","VP -> C","OS -> C","FN -> B","NB -> V","BB -> C","BK -> V","VF -> V","VC -> O","NO -> K","KF -> P","FO -> C","OB -> K","ON -> S","BO -> V","KV -> H","CC -> O","HF -> N","VS -> S","PN -> P","SK -> F","PO -> V","HH -> F","VV -> N","VH -> N","SV -> S","CV -> B","KS -> K","PS -> V","OV -> S","SB -> V","NP -> K","SN -> C","NK -> O","PK -> F","VN -> P","PP -> K","VB -> C","OP -> P"]
