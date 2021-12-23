module AdventOfCode.Y2021.Day21 where

import AdventOfCode.Common.Tuple
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

data Player = Player
   { ident :: Int
   , position :: Int
   , score :: Int
   } deriving (Show, Eq)

data DeterministicDie = DeterministicDie
   { nextRoll :: Int
   , maxRoll :: Int
   , totalRolls :: Int
   } deriving (Show, Eq)

data Game = Game
   { player1 :: Player
   , player2 :: Player
   , die :: DeterministicDie
   } deriving (Show, Eq)

type Wins = (Integer,Integer)
(a,b) <+> (c,d) = (a+c, b+d)

--------------------------------------------------------------------------------

roll :: DeterministicDie -> (Int, DeterministicDie)
roll d = (nextRoll d, d
   { nextRoll = if nextRoll d == maxRoll d then 1 else nextRoll d + 1
   , totalRolls = totalRolls d + 1 })

mod10 :: Int -> Int
mod10 n = ((n-1) `mod` 10) + 1

advance :: Int -> Player -> Player
advance n player = let
   newPos = mod10 (position player + n)
   in player { position = newPos, score = score player + newPos }

doTurn :: Int -> Game -> Game
doTurn playerId game = let
   (r1,d1) = roll $ die game
   (r2,d2) = roll d1
   (r3,d3) = roll d2
   f p = if playerId == ident p then advance (r1+r2+r3) p else p
   in game { player1 = f (player1 game), player2 = f (player2 game), die = d3 }

winner :: Player -> Bool
winner player = score player >= 1000

complete :: Game -> Bool
complete game = winner (player1 game) || winner (player2 game)

doGame :: Game -> Game
doGame game = head $ dropWhile (not . complete) $ aux game
   where 
   aux :: Game -> [Game]
   aux g = let g1 = doTurn 1 g in g : g1 : aux (doTurn 2 g1)

part1 :: Game -> Int
part1 game = let
   endState = doGame game
   loserScore = min (score $ player1 endState) (score $ player2 endState)
   dieRolls = totalRolls $ die endState
   in loserScore * dieRolls

--------------------------------------------------------------------------------

quantumDieRoll :: [Int]
quantumDieRoll = [1,2,3]

winner2 :: Player -> Bool
winner2 player = score player >= winningScore

winningScore = 21

turnTheTables :: Game -> Game
turnTheTables game = game { player1 = player2 game, player2 = player1 game }

doGame2 :: Game -> Wins
doGame2 = aux' where
   
   mem :: Map (Int,Int,Int,Int) Wins
   mem = M.fromList $ do 
      p1s <- [0.. winningScore + 9]
      p1p <- [1..10]
      p2s <- [0.. winningScore + 9]
      p2p <- [1..10]
      return ((p1p,p1s,p2p,p2s), aux $ Game (Player 1 p1p p1s) (Player 2 p2p p2s) undefined)
   
   aux :: Game -> Wins
   aux game
      | winner2 (player1 game) = (1,0)
      | winner2 (player2 game) = (0,1)
      | otherwise = let
         rolls = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]
         advancedPlayers = map (\(roll,freq) -> (advance roll (player1 game), freq)) rolls
         outcomes = map (\(ap,freq) -> (*freq) $# (aux' $ turnTheTables $ game {player1 = ap})) advancedPlayers
         in swap $ foldr1 (<+>) outcomes
   
   aux' :: Game -> Wins
   aux' game = let
      p1 = player1 game
      p2 = player2 game
      key = (position p1, score p1, position p2, score p2)
      in fromJust $ M.lookup key mem

part2 :: Game -> Integer
part2 game = uncurry max $ doGame2 game

--------------------------------------------------------------------------------

sampleInput :: Game
sampleInput = Game (Player 1 4 0) (Player 2 8 0) (DeterministicDie 1 100 0)

myInput :: Game
myInput = Game (Player 1 5 0) (Player 2 8 0) (DeterministicDie 1 100 0)
