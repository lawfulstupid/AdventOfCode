module AdventOfCode.Common.Vector3 where

import AdventOfCode.Common.Matrix

import Data.List

--------------------------------------------------------------------------------

data Vector a = Vec
   { x :: a
   , y :: a
   , z :: a
   } deriving (Eq)

instance Show a => Show (Vector a) where
   show (Vec x y z) = show (x,y,z)

instance Num a => Num (Vector a) where
   Vec x1 y1 z1 + Vec x2 y2 z2 = Vec (x1+x2) (y1+y2) (z1+z2)
   negate (Vec x y z) = Vec (-x) (-y) (-z)

--------------------------------------------------------------------------------

data Axis = X | Y | Z
   deriving (Show, Eq, Enum, Bounded)

type Turns = Int

--------------------------------------------------------------------------------

apply :: Num a => Matrix a -> Vector a -> Vector a
apply m (Vec x y z) = let
   [[a,b,c],[d,e,f],[g,h,i]] = unpack m
   in Vec (a*x+b*y+c*z) (d*x+e*y+f*z) (g*x+h*y+i*z)

rotation :: Num a => Axis -> Turns -> Matrix a
rotation axis n = let
   s = case n `mod` 4 of { 0 -> 0; 1 -> 1; 2 -> 0; 3 -> -1 }
   c = case n `mod` 4 of { 0 -> 1; 1 -> 0; 2 -> -1; 3 -> 0 }
   ifAxis ax t f = if axis == ax then t else f
   in Grid
      [ [ifAxis X 1 c,     ifAxis Z (-s) 0,  ifAxis Y s 0    ]
      , [ifAxis Z s 0,     ifAxis Y 1 c,     ifAxis X (-s) 0 ]
      , [ifAxis Y (-s) 0,  ifAxis X s 0,     ifAxis Z 1 c    ] ]

rotations :: (Num a, Eq a) => [Matrix a]
rotations = nub $ do
   turnsX <- [0..3]
   turnsY <- [0..3]
   turnsZ <- [0..3]
   return (rotation X turnsX * rotation Y turnsY * rotation Z turnsZ)
