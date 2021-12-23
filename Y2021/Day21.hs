module AdventOfCode.Y2021.Day21 where

data Player = Player
   { ident :: Int
   , position :: Int
   , score :: Int
   } deriving (Show, Eq)

data Die = Die
   { nextRoll :: Int
   , maxRoll :: Int
   , totalRolls :: Int
   } deriving (Show, Eq)

data Game = Game
   { player1 :: Player
   , player2 :: Player
   , die :: Die
   } deriving (Show, Eq)

--------------------------------------------------------------------------------

roll :: Die -> (Int, Die)
roll d = (nextRoll d, d
   { nextRoll = if nextRoll d == maxRoll d then 1 else nextRoll d + 1
   , totalRolls = totalRolls d + 1
   })

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

sampleInput :: Game
sampleInput = Game (Player 1 4 0) (Player 2 8 0) (Die 1 100 0)

myInput :: Game
myInput = Game (Player 1 5 0) (Player 2 8 0) (Die 1 100 0)
