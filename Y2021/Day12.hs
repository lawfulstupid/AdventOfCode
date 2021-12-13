{-# LANGUAGE TupleSections #-}

module AdventOfCode.Y2021.Day12 where

import AdventOfCode.Common.Parser

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.List
import Data.Maybe
import Data.Char

--------------------------------------------------------------------------------

type Cave = String

data Tunnel = Tunnel Cave Cave

endpoints :: Tunnel -> [Cave]
endpoints (Tunnel a b) = [a,b]

instance Eq Tunnel where
   Tunnel a b == Tunnel c d = (a == c && b == d) || (a == d && b == c)

instance Show Tunnel where
   show (Tunnel a b) = a ++ "-" ++ b


data CaveData = CaveData
   { visited :: Bool
   } deriving (Show)
defaultCaveData = CaveData False


data CaveSystem = CaveSystem
   { caves :: Map Cave CaveData
   , tunnels :: [Tunnel]
   } deriving (Show)


type Path = [Cave]

--------------------------------------------------------------------------------

isBig :: Cave -> Bool
isBig = all isUpper

canEnter :: CaveSystem -> Cave -> Bool
canEnter g v = isBig v || (not $ visited $ fromJust $ M.lookup v $ caves g)

isEndpointOf :: Cave -> Tunnel -> Bool
isEndpointOf v e = elem v $ endpoints e

exits :: Cave -> CaveSystem -> [Cave]
exits s g = let
   exitTunnels = filter (s `isEndpointOf`) $ tunnels g
   exitCaves = filter (/= s) $ foldMap endpoints exitTunnels
   validExitCaves = filter (canEnter g) exitCaves
   in validExitCaves

paths :: Cave -> Cave -> CaveSystem -> [Path]
paths s t _ | s == t = [[t]]
paths s t g = do
   let g' = g { caves = M.insert s (CaveData True) $ caves g }
   x <- exits s g'
   p <- paths x t g'
   return (s:p)

part1 :: CaveSystem -> Int
part1 = length . paths "start" "end"

--------------------------------------------------------------------------------

sampleInputSmall :: CaveSystem
sampleInputSmall = makeCaveSystem ["start-A","start-b","A-c","A-b","b-d","A-end","b-end"]

sampleInputMedium :: CaveSystem
sampleInputMedium = makeCaveSystem ["dc-end","HN-start","start-kj","dc-start","dc-HN","LN-dc","HN-end","kj-sa","kj-HN","kj-dc"]
   
sampleInputLarge :: CaveSystem
sampleInputLarge = makeCaveSystem ["fs-end","he-DX","fs-he","start-DX","pj-DX","end-zg","zg-sl","zg-pj","pj-he","RW-he","fs-DX","pj-RW","zg-RW","start-pj","he-WI","zg-he","pj-fs","start-RW"]

myInput :: CaveSystem
myInput = makeCaveSystem ["QF-bw","end-ne","po-ju","QF-lo","po-start","XL-ne","bw-US","ne-lo","nu-ne","bw-po","QF-ne","ne-ju","start-lo","lo-XL","QF-ju","end-ju","XL-end","bw-ju","nu-start","lo-nu","nu-XL","xb-XL","XL-po"]

makeCaveSystem :: [String] -> CaveSystem
makeCaveSystem input = let
   tunnels = map (parseUsing tunnelParser) input
   caves = nub $ sort $ foldMap endpoints tunnels
   in CaveSystem (M.fromList $ map (,defaultCaveData) caves) tunnels

tunnelParser :: Parser Tunnel
tunnelParser = do
   a <- string
   match "-"
   b <- string
   return $ Tunnel a b
