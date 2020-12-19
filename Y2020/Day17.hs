{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE KindSignatures #-}

module AdventOfCode.Y2020.Day17 where

import Prelude hiding (cycle)
import AdventOfCode.Y2020.Common

import Control.Monad

import Data.List hiding (cycle)
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S

newtype Vector3 a = V3 (a,a,a)   deriving (Eq, Ord)
newtype Vector4 a = V4 (a,a,a,a) deriving (Eq,Ord)

type Range a = (a,a)
type Dimensions v a = v (Range a)
type State = Bool
data Grid v = Grid (Set (v Int))

-- instance Show Grid where
   -- show grid = intercalate "\n\n" $ map showSlice $ range zr
      -- where
      -- !(xr,yr,zr) = dim grid
      -- showSlice :: Int -> String
      -- showSlice z = "z=" ++ show z ++ (range yr >>= ("\n" ++) . showRow)
         -- where
         -- showRow :: Int -> String
         -- showRow y = range xr >>= \x -> showState $ get (x,y,z) grid

class (Ord (v Int)) => Vec (v :: * -> *) where
   zero :: Num a => v a
   neighbours :: (Eq a, Num a, Enum a) => v a -> [v a]
   from2d :: Num a => (a,a) -> v a
   bounds :: Ord a => Set (v a) -> Dimensions v a
   vmap :: (a -> b) -> v a -> v b
   iterateInRange :: Enum a => Dimensions v a -> [v a]

instance Vec Vector3 where
   zero = V3 (0,0,0)
   neighbours (V3 (x,y,z)) = do
      i <- [-1..1]
      j <- [-1..1]
      k <- [-1..1]
      guard (V3 (i,j,k) /= zero)
      return $ V3 (x+i,y+j,z+k)
   from2d (x,y) = V3 (x,y,0)
   bounds set = let
      !xr = minMax $ S.map (\(V3 (x,y,z)) -> x) set
      !yr = minMax $ S.map (\(V3 (x,y,z)) -> y) set
      !zr = minMax $ S.map (\(V3 (x,y,z)) -> z) set
      in V3 (xr,yr,zr)
   vmap f (V3 (x,y,z)) = V3 (f x, f y, f z)
   iterateInRange (V3 (xr,yr,zr)) = do
      x <- range xr
      y <- range yr
      z <- range zr
      return $ V3 (x,y,z)

instance Vec Vector4 where
   zero = V4 (0,0,0,0)
   neighbours (V4 (x,y,z,w)) = do
      i <- [-1..1]
      j <- [-1..1]
      k <- [-1..1]
      l <- [-1..1]
      guard (V4 (i,j,k,l) /= zero)
      return $ V4 (x+i,y+j,z+k,w+l)
   from2d (x,y) = V4 (x,y,0,0)
   bounds set = let
      !xr = minMax $ S.map (\(V4 (x,y,z,w)) -> x) set
      !yr = minMax $ S.map (\(V4 (x,y,z,w)) -> y) set
      !zr = minMax $ S.map (\(V4 (x,y,z,w)) -> z) set
      !wr = minMax $ S.map (\(V4 (x,y,z,w)) -> w) set
      in V4 (xr,yr,zr,wr)
   vmap f (V4 (x,y,z,w)) = V4 (f x, f y, f z, f w)
   iterateInRange (V4 (xr,yr,zr,wr)) = do
      x <- range xr
      y <- range yr
      z <- range zr
      w <- range wr
      return $ V4 (x,y,z,w)

showState :: State -> String
showState True = "#"
showState False = "."

readState :: Char -> State
readState '#' = True
readState '.' = False

isIn :: Ord a => a -> Range a -> Bool
isIn x (a,b) = a <= x && x <= b

range :: Enum a => Range a -> [a]
range (a,b) = [a..b]

expand :: Num a => Range a -> Range a
expand (a,b) = (a-1,b+1)

get :: Vec v => v Int -> Grid v -> State
get pos (Grid set) = S.member pos set

getNeighbourStates :: Vec v => v Int -> Grid v -> [State]
getNeighbourStates pos grid = map (flip get grid) $ neighbours pos

dim :: Vec v => Grid v -> Dimensions v Int
dim (Grid set) = bounds set

minMax :: Ord a => Set a -> (a,a)
minMax xs = (S.findMin xs, S.findMax xs)

cycle :: Vec v => Grid v -> Grid v
cycle grid = let
   !dimensions = vmap expand $ dim grid
   !set = S.fromList $ do
      pos <- iterateInRange dimensions
      let !currState = get pos grid
      let !activegetNeighbourStates = count id $ getNeighbourStates pos grid
      let !nextState = if currState then activegetNeighbourStates `elem` [2,3] else activegetNeighbourStates == 3
      if nextState then [pos] else []
   in Grid set

countActive :: Grid v -> Int
countActive (Grid set) = S.size set

part1 input = let
   !grid = fromInput input :: Grid Vector3
   !final = foldr id grid $ replicate 6 cycle
   in countActive final

part2 input = let
   !grid = fromInput input :: Grid Vector4
   !final = foldr id grid $ replicate 6 cycle
   in countActive final


type Input = [String]

fromInput :: Vec v => Input -> Grid v
fromInput grid = let
   !xr = (0, length (grid !! 0) - 1)
   !yr = (0, length grid - 1)
   !set = S.fromList $ do
      x <- range xr
      y <- range yr
      let cell = grid !! y !! x
      guard (cell == '#')
      return $ from2d (x,y)
   in Grid set where

sample :: Input
sample =
   [ ".#."
   , "..#"
   , "###" ]

myInput :: Input
myInput =
   [ "#....#.#"
   , "..##.##."
   , "#..#..#."
   , ".#..#..#"
   , ".#..#..."
   , "##.#####"
   , "#..#..#."
   , "##.##..#" ]
