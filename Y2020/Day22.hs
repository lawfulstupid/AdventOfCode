module AdventOfCode.Y2020.Day22 where

--------------------------------------------------------------------------------

type Card = Int
type Deck = [Card]
data Game = Game
   { player1 :: Deck
   , player2 :: Deck
   } deriving (Eq, Show)

--------------------------------------------------------------------------------

advanceRound :: Game -> Either Deck Game
advanceRound (Game p1 []) = Left p1
advanceRound (Game [] p2) = Left p2
advanceRound (Game (a:p1) (b:p2)) = if a < b
   then Right $ Game p1 (p2 ++ [b,a])
   else Right $ Game (p1 ++ [a,b]) p2

play :: Game -> Deck
play game = case advanceRound game of
   Left deck -> deck
   Right game' -> play game'

score :: Deck -> Int
score deck = sum $ zipWith (*) [1..] $ reverse deck

--------------------------------------------------------------------------------

part1 :: Game -> Int
part1 game = score $ play game

--------------------------------------------------------------------------------

sampleInput :: Game
sampleInput = Game [9,2,6,3,1] [5,8,4,7,10]

myInput :: Game
myInput = Game [14,6,21,10,1,33,7,13,25,8,17,11,28,27,50,2,35,49,19,46,3,38,23,5,43] [18,9,12,39,48,24,32,45,47,41,40,15,22,36,30,26,42,34,20,16,4,31,37,44,29]
