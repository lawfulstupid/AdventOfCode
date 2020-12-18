module AdventOfCode.Y2020.Word36 where

import GHC.Word
import Data.Bits

data Word36 = W36 Word64
   deriving (Eq, Ord)

to36 :: Word64 -> Word36
to36 n = W36 (n `mod` (2^36 - 1))

instance Show Word36 where
   show (W36 x) = show x

instance Read Word36 where
   readsPrec n s = do
      (x,r) <- readsPrec n s
      return (to36 x, r)

instance Enum Word36 where
   fromEnum (W36 x) = fromEnum x
   toEnum = to36 . toEnum

instance Num Word36 where
   (W36 x) + (W36 y) = to36 (x+y)
   (W36 x) * (W36 y) = to36 (x*y)
   (W36 x) - (W36 y) = to36 (x-y)
   abs (W36 x) = to36 $ abs x
   signum (W36 x) = to36 $ signum x
   fromInteger n = to36 $ fromInteger n

instance Real Word36 where
   toRational (W36 x) = toRational x

instance Bounded Word36 where
   minBound = 0
   maxBound = 2^36 - 1

instance Integral Word36 where
   quotRem (W36 x) (W36 y) = let (q,r) = quotRem x y in (to36 q, to36 r)
   toInteger (W36 x) = toInteger x

instance Bits Word36 where
   (W36 x) .&. (W36 y) = to36 (x .&. y)
   (W36 x) .|. (W36 y) = to36 (x .|. y)
   xor (W36 x) (W36 y) = to36 (xor x y)
   complement (W36 x)  = to36 $ complement x
   shift (W36 x) n     = to36 $ shift x n
   rotate (W36 x) n    = to36 $ rotate x n
   bitSize _           = 36
   bitSizeMaybe _      = Just 36
   isSigned _          = False
   testBit (W36 x) n   = testBit x n
   bit n               = to36 $ bit n
   popCount (W36 x)    = popCount x