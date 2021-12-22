{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module AdventOfCode.Common.Matrix (
   module AdventOfCode.Common.Matrix,
   module AdventOfCode.Common.Grid
) where

import AdventOfCode.Common.Grid

type Matrix a = Grid a

instance Num a => Num (Matrix a) where
   fromInteger n = Grid [[fromInteger n]]
   negate = fmap negate
   
   a + b = if dimensions a /= dimensions b
      then error "dimensions do not match"
      else Grid $ zipWith (zipWith (+)) (unpack a) (unpack b)
   
   a * b = let
      (wa,ha) = dimensions a
      (wb,hb) = dimensions b
      f (x,y) = sum [(a # (k,y)) * (b # (x,k)) | k <- [0..ha-1]]
      in if ha /= wb
         then error "dimensions do not match"
         else f <$> coordGrid (wa,hb)

isSquare :: Matrix a -> Bool
isSquare = uncurry (==) . dimensions

det :: Num a => Matrix a -> a
det m | not $ isSquare m = error "cannot compute determinant of non-square matrix"
det m | dimensions m == (1,1) = m # (0,0)
det m = let
   parity x = if x `mod` 2 == 0 then 1 else -1
   term x = parity x * (m # (x,0)) * det (minor (x,0) m)
   in sum $ map term [0 .. width m - 1]

minor :: Coords -> Matrix a -> Matrix a
minor (x,y) = deleteRow y . deleteCol x

zero :: Num a => Int -> Matrix a
zero n = const 0 <$> coordGrid (n,n)

ident :: Num a => Int -> Matrix a
ident n = (\(x,y) -> if x == y then 1 else 0) <$> coordGrid (n,n)
