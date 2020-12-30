{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module AdventOfCode.Y2019.Day22 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Monoid

type Card = Int
type Deck = [Card]
type DeckSize = Int
type Shuffle = DeckSize -> (Int -> Int)

data Mapping = Mapping {apply :: Shuffle}

instance Semigroup Mapping where
   Mapping f <> Mapping g = Mapping $ \deckSize -> (f deckSize . g deckSize)

instance Monoid Mapping where
   mempty = Mapping $ const id

deck :: Int -> Deck
deck n = [0..n-1]

dealNew :: Shuffle
dealNew deckSize idx = deckSize - idx - 1

cut :: Int -> Shuffle
cut n deckSize idx = (idx + n) `mod` deckSize

-- assumes n and deckSize are coprime
deal :: Int -> Shuffle
deal n deckSize idx = let
   !x = fromJust $ find (\j -> j `mod` n == 0) $ iterate (+deckSize) idx
   in x `div` n

flatten :: [Shuffle] -> Shuffle
flatten = apply . mconcat . map Mapping

part1 :: Int
part1 = let
   !f = flatten myInput
   xs = map (f 10007) $ deck 10007
   in fromJust $ findIndex (==2019) xs

samples :: [[Shuffle]]
samples =
   [ [deal 7, dealNew, dealNew]
   , [cut 6, deal 7, dealNew]
   , [deal 7, deal 9, cut (-2)]
   , [dealNew, cut (-2), deal 7, cut 8, cut (-4), deal 7, cut 3, deal 9, deal 3, cut (-1)] ]

myInput :: [Shuffle]
myInput =
   [ deal 24, cut (-9655), deal 20, cut (-3052), deal 14, dealNew
   , deal 12, cut 2041, dealNew, deal 13, cut (-5574), dealNew, deal 52
   , cut 2735, deal 14, dealNew, deal 72, dealNew, deal 11, cut (-7008)
   , deal 7, cut (-3920), dealNew, deal 68, cut (-7497), deal 7
   , cut 8878, deal 39, cut (-3407), deal 74, cut (-3728), dealNew
   , cut 483, deal 55, cut 8147, deal 48, cut 5734, deal 35, dealNew
   , deal 53, dealNew, cut 9833, deal 21, cut (-1328), deal 29, cut 469
   , deal 34, dealNew, deal 50, cut 8218, deal 8, cut 1546, deal 27
   , cut 3699, deal 44, cut 1167, dealNew, cut (-9744), deal 71
   , cut (-6111), deal 19, cut 2592, deal 17, cut 3257, deal 11
   , cut 4618, deal 64, dealNew, cut (-1513), dealNew, cut (-2976)
   , deal 58, cut 2744, deal 4, cut 6408, deal 66, cut 5182, deal 6
   , cut (-1767), deal 12, cut (-7805), deal 45, cut (-4126), deal 52
   , cut 2112, deal 35, cut 863, deal 55, cut 3159, deal 67, dealNew
   , deal 65, cut (-3194), deal 69, cut 5695, deal 7, cut (-5035)
   , deal 30, cut (-2282), deal 70 ]
