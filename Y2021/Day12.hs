{-# LANGUAGE TupleSections #-}

module AdventOfCode.Y2021.Day12 where

import AdventOfCode.Common.Parser

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


data CaveSystem = CaveSystem
   { tunnels :: [Tunnel]
   , state :: State
   } deriving (Show)


data State = State
   { visitedCaves :: [Cave]
   , doubleVisitDone :: Bool
   } deriving (Show)

defaultState :: State
defaultState = State [] False

type Path = [Cave]

--------------------------------------------------------------------------------

isBig :: Cave -> Bool
isBig = all isUpper

visited :: CaveSystem -> Cave -> Bool
visited g v = elem v $ visitedCaves $ state g

canEnter :: CaveSystem -> Cave -> Bool
canEnter g v = isBig v || (not $ visited g v)

addVisit :: CaveSystem -> Cave -> CaveSystem
addVisit g v = g {state = (state g) {visitedCaves = visitedCaves (state g) `union` [v]}}

isEndpointOf :: Cave -> Tunnel -> Bool
isEndpointOf v e = elem v $ endpoints e

exits :: VisitFunctions -> Cave -> CaveSystem -> [Cave]
exits fs@(query,update) s g = let
   exitTunnels = filter (s `isEndpointOf`) $ tunnels g
   exitCaves = filter (/= s) $ foldMap endpoints exitTunnels
   validExitCaves = filter (query g) exitCaves
   in validExitCaves

paths :: VisitFunctions -> Cave -> Cave -> CaveSystem -> [Path]
paths fs@(query,update) s t _ | s == t = [[t]]
paths fs@(query,update) s t g = do
   let g' = update g s
   x <- exits fs s g'
   p <- paths fs x t g'
   return (s:p)


type VisitFunctions = (CaveSystem -> Cave -> Bool, CaveSystem -> Cave -> CaveSystem)
p1VisitFunctions :: VisitFunctions
p1VisitFunctions = (canEnter, addVisit)

part1 :: CaveSystem -> Int
part1 = length . paths p1VisitFunctions "start" "end"

--------------------------------------------------------------------------------

canEnter2 :: CaveSystem -> Cave -> Bool
canEnter2 g v = canEnter g v
   || (not $ doubleVisitDone $ state g) && (not $ elem v ["start", "end"])

addVisit2 :: CaveSystem -> Cave -> CaveSystem
addVisit2 g v = if canEnter g v
   then addVisit g v
   else g {state = (state g) {doubleVisitDone = True}}

p2VisitFunctions :: VisitFunctions
p2VisitFunctions = (canEnter2, addVisit2)

part2 :: CaveSystem -> Int
part2 = length . paths p2VisitFunctions "start" "end"

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
   in CaveSystem tunnels defaultState

tunnelParser :: Parser Tunnel
tunnelParser = do
   a <- string
   match "-"
   b <- string
   return $ Tunnel a b
