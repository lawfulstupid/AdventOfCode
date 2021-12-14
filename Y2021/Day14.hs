module AdventOfCode.Y2021.Day14 where

import AdventOfCode.Common.Parser
import AdventOfCode.Common.List
import Data.List

--------------------------------------------------------------------------------

type Polymer = String

data Rule = Rule
   { input :: String
   , inject :: Char
   } deriving (Eq)

data PolymerTemplate = PolymerTemplate
   { polymer :: Polymer
   , rules :: [Rule] }

--------------------------------------------------------------------------------

applyRules :: [Rule] -> (Char, Char) -> String
applyRules rs (a,b) = case find (\r -> input r == [a,b]) rs of
   Nothing -> [a]
   Just r  -> [a, inject r]

step :: PolymerTemplate -> PolymerTemplate
step (PolymerTemplate p rs) = PolymerTemplate (aux p) rs where
   aux :: Polymer -> Polymer
   aux [] = []
   aux [x] = [x]
   aux (x:y:xs) = applyRules rs (x,y) ++ aux (y:xs)

doStep :: Int -> PolymerTemplate -> PolymerTemplate
doStep n pt = iterate step pt !! n

differential :: Polymer -> Int
differential p = let freqs = map snd $ freq p in maximum freqs - minimum freqs

part1 :: PolymerTemplate -> Int
part1 = differential . polymer . doStep 10

--------------------------------------------------------------------------------

ruleParser :: Parser Rule
ruleParser = do
   l <- char
   r <- char
   match " -> "
   i <- char
   return $ Rule [l,r] i

makeTemplate :: String -> [String] -> PolymerTemplate
makeTemplate p rs = PolymerTemplate p $ map (parseUsing ruleParser) rs

sampleInput :: PolymerTemplate
sampleInput = makeTemplate "NNCB" ["CH -> B","HH -> N","CB -> H","NH -> C","HB -> C","HC -> B","HN -> C","NN -> C","BH -> H","NC -> B","NB -> B","BN -> B","BB -> N","BC -> B","CC -> N","CN -> C"]

myInput :: PolymerTemplate
myInput = makeTemplate "ONHOOSCKBSVHBNKFKSBK" ["HO -> B","KB -> O","PV -> B","BV -> C","HK -> N","FK -> H","NV -> C","PF -> K","FV -> B","NH -> P","CO -> N","HV -> P","OH -> H","BC -> H","SP -> C","OK -> F","KH -> N","HB -> V","FP -> N","KP -> O","FB -> O","FH -> F","CN -> K","BP -> P","SF -> O","CK -> K","KN -> O","VK -> C","HP -> N","KK -> V","KO -> C","OO -> P","BH -> B","OC -> O","HC -> V","HS -> O","SH -> V","SO -> C","FS -> N","CH -> O","PC -> O","FC -> S","VO -> H","NS -> H","PH -> C","SS -> F","BN -> B","BF -> F","NC -> F","CS -> F","NN -> O","FF -> P","OF -> H","NF -> O","SC -> F","KC -> F","CP -> H","CF -> K","BS -> S","HN -> K","CB -> P","PB -> V","VP -> C","OS -> C","FN -> B","NB -> V","BB -> C","BK -> V","VF -> V","VC -> O","NO -> K","KF -> P","FO -> C","OB -> K","ON -> S","BO -> V","KV -> H","CC -> O","HF -> N","VS -> S","PN -> P","SK -> F","PO -> V","HH -> F","VV -> N","VH -> N","SV -> S","CV -> B","KS -> K","PS -> V","OV -> S","SB -> V","NP -> K","SN -> C","NK -> O","PK -> F","VN -> P","PP -> K","VB -> C","OP -> P"]
