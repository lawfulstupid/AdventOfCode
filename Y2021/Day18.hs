module AdventOfCode.Y2021.Day18 where

import AdventOfCode.Common.Parser
import AdventOfCode.Common.List (substringIndex)

import Control.Applicative

import Data.Maybe
import Data.List
import Data.Char

--------------------------------------------------------------------------------

data SFN = SFN SFN SFN | Lit Int | Explosion (Int,Int)
   deriving (Eq)

unlit :: SFN -> Int
unlit (Lit n) = n

instance Show SFN where
   show (Lit n) = show n
   show (SFN a b) = "[" ++ show a ++ "," ++ show b ++ "]"
   show (Explosion e) = "*"

instance Read SFN where
   readsPrec n = apply sfnParser

instance Num SFN where
   a + b = reduce $ SFN a b

--------------------------------------------------------------------------------

reduce :: SFN -> SFN
reduce sfn = maybe sfn reduce (explode sfn <|> split sfn)

groupByNumber :: String -> [String]
groupByNumber = groupBy (\x y -> isDigit x == isDigit y)

addToFirstNum :: Int -> [String] -> [String]
addToFirstNum n [] = []
addToFirstNum n (g:gs) = if isDigit $ head g then show (n + read g) : gs else g : addToFirstNum n gs

explode :: SFN -> Maybe SFN
explode sfn = findExplosion 0 sfn >>= \sfn' -> let
   (a,b) = fromJust $ getExplosionParams sfn'
   str = show sfn'
   idx = fromJust $ elemIndex '*' str
   left = concat $ reverse $ addToFirstNum a $ reverse $ groupByNumber $ take idx str
   right = concat $ addToFirstNum b $ groupByNumber $ drop (idx+1) str
   in return $ read (left ++ "0" ++ right)
   where
   
   findExplosion :: Int -> SFN -> Maybe SFN
   findExplosion _ (Lit n) = Nothing
   findExplosion n (SFN a b) = if n >= 4
      then Just $ Explosion (unlit a, unlit b)
      else case findExplosion (n+1) a of
         Just e -> Just (SFN e b)
         Nothing -> case findExplosion (n+1) b of
            Just e -> Just (SFN a e)
            Nothing -> Nothing

   getExplosionParams :: SFN -> Maybe (Int,Int)
   getExplosionParams (Lit _) = Nothing
   getExplosionParams (Explosion e) = Just e
   getExplosionParams (SFN a b) = getExplosionParams a <|> getExplosionParams b

split :: SFN -> Maybe SFN
split sfn = (read . concat) <$> (aux $ groupByNumber $ show sfn) where
   aux :: [String] -> Maybe [String]
   aux [] = Nothing
   aux (g:gs) = let
      isNum = isDigit $ head g
      n = read g
      a = n `div` 2
      b = n - a
      elem = "[" ++ show a ++ "," ++ show b ++ "]"
      in if isNum && n >= 10 then Just (elem : gs) else (g:) <$> aux gs

sumSFN :: [SFN] -> SFN
sumSFN = foldl1' (+)

magnitude :: SFN -> Int
magnitude (Lit n) = n
magnitude (SFN a b) = 3 * magnitude a + 2 * magnitude b

part1 :: [SFN] -> Int
part1 = magnitude . sumSFN

--------------------------------------------------------------------------------

part2 :: [SFN] -> Int
part2 input = maximum $ do
   a <- input
   b <- input
   return $ magnitude (a + b)

--------------------------------------------------------------------------------

sfnParser :: Parser SFN
sfnParser = (Lit <$> reader) <|> do
   match "["
   a <- sfnParser
   match ","
   b <- sfnParser
   match "]"
   return $ SFN a b

sampleInput1 :: [SFN]
sampleInput1 = map read ["[1,1]","[2,2]","[3,3]","[4,4]"]

sampleInput2 :: [SFN]
sampleInput2 = map read ["[1,1]","[2,2]","[3,3]","[4,4]","[5,5]"]

sampleInput3 :: [SFN]
sampleInput3 = map read ["[1,1]","[2,2]","[3,3]","[4,4]","[5,5]", "[6,6]"]

sampleInput4 :: [SFN]
sampleInput4 = map read
   [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
   , "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
   , "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
   , "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
   , "[7,[5,[[3,8],[1,4]]]]"
   , "[[2,[2,2]],[8,[8,1]]]"
   , "[2,9]"
   , "[1,[[[9,3],9],[[9,0],[0,7]]]]"
   , "[[[5,[7,4]],7],1]"
   , "[[[[4,2],2],6],[8,7]]" ]

sampleInput5 :: [SFN]
sampleInput5 = map read
   [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
   , "[[[5,[2,8]],4],[5,[[9,9],0]]]"
   , "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
   , "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
   , "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
   , "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
   , "[[[[5,4],[7,7]],8],[[8,3],8]]"
   , "[[9,3],[[9,9],[6,[4,9]]]]"
   , "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
   , "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]" ]

myInput :: [SFN]
myInput = map read
   [ "[[8,8],5]"
   , "[[[[9,0],1],4],[[3,6],[0,5]]]"
   , "[[9,[0,[4,5]]],[1,[[6,8],4]]]"
   , "[[8,7],[[[8,5],[2,0]],[[6,3],[5,0]]]]"
   , "[[[1,8],2],[[[9,1],[2,0]],[1,[9,4]]]]"
   , "[[[6,[8,8]],[6,4]],[[8,2],[[0,8],9]]]"
   , "[[[6,3],[9,[9,1]]],[[0,0],1]]"
   , "[[[[2,7],[8,2]],[[9,6],[5,1]]],[[[7,6],[6,0]],[4,2]]]"
   , "[[[8,[9,1]],[9,3]],[[[5,4],[8,0]],[[3,5],[9,5]]]]"
   , "[[[3,[4,9]],2],[[7,9],7]]"
   , "[[[7,[9,0]],5],[[[3,4],[2,6]],[[3,5],[7,2]]]]"
   , "[[8,[8,9]],[[[3,2],[6,2]],4]]"
   , "[[[[8,0],3],[3,8]],[[[5,0],[7,3]],[5,[3,0]]]]"
   , "[4,[[3,[0,9]],[[5,0],[2,0]]]]"
   , "[[[[0,1],5],[3,[9,6]]],[[[4,4],5],[[3,8],[5,1]]]]"
   , "[[[[4,8],8],0],[5,[[1,7],[4,3]]]]"
   , "[[3,[[1,1],[5,6]]],[7,[[4,0],[0,7]]]]"
   , "[9,[4,[[1,3],2]]]"
   , "[[[1,[2,7]],[[4,7],3]],[2,1]]"
   , "[[[9,5],[2,5]],[[[8,9],[4,5]],2]]"
   , "[[2,[[7,4],6]],[[1,[0,7]],[[4,8],8]]]"
   , "[[[[0,5],3],[7,0]],9]"
   , "[[[[1,4],[4,3]],7],[[9,4],[6,[8,6]]]]"
   , "[[[7,2],[[3,3],1]],[5,9]]"
   , "[[[9,[6,2]],2],[[6,5],6]]"
   , "[[5,[3,2]],[[[2,4],[1,5]],[6,3]]]"
   , "[6,3]"
   , "[[9,6],[[[8,2],[5,6]],[[3,5],[3,3]]]]"
   , "[[[[2,5],7],4],[8,3]]"
   , "[[[[6,1],9],[0,6]],[6,2]]"
   , "[[[[8,4],2],[[0,1],[5,8]]],9]"
   , "[[[7,0],[4,9]],[[[9,9],[4,4]],[6,6]]]"
   , "[[[9,8],[2,0]],[[9,[6,2]],[6,[5,6]]]]"
   , "[[[9,8],[[0,6],[3,5]]],[[[4,7],[7,5]],[7,[8,5]]]]"
   , "[[[[9,0],[1,6]],[2,[5,3]]],[[[2,0],[0,3]],[[9,1],[7,7]]]]"
   , "[[[5,[2,2]],[2,[1,0]]],[1,1]]"
   , "[[[9,[7,2]],[[2,7],1]],[[5,7],[[8,7],7]]]"
   , "[[[9,[9,4]],[[0,8],2]],[0,[[2,2],[4,1]]]]"
   , "[[[5,5],[9,[2,0]]],[[[9,0],6],1]]"
   , "[[[1,9],[[9,5],[5,6]]],[6,[5,[9,4]]]]"
   , "[[[[8,6],9],9],[[7,2],[7,[2,6]]]]"
   , "[[[[6,4],7],7],[[2,[9,7]],7]]"
   , "[[7,[[5,6],9]],[[[9,8],8],[[8,9],[1,0]]]]"
   , "[[[0,[7,6]],0],[[[2,5],1],9]]"
   , "[[[3,[4,1]],[4,2]],[0,[[6,0],[1,6]]]]"
   , "[[9,[0,0]],[[[3,0],[9,9]],[1,[1,5]]]]"
   , "[[[[9,9],1],6],[5,6]]"
   , "[3,4]"
   , "[[[[5,4],9],6],2]"
   , "[[5,4],[[6,[7,4]],[[0,3],0]]]"
   , "[[[3,[9,6]],4],[[[9,8],6],3]]"
   , "[[5,[1,[5,5]]],[[[3,8],[0,1]],[[9,3],[6,2]]]]"
   , "[[4,[0,3]],1]"
   , "[[[7,[2,9]],[[5,8],2]],[[[4,4],[2,0]],8]]"
   , "[[[[4,0],0],8],7]"
   , "[[[[3,0],0],[[6,0],3]],[[[1,5],1],[3,[0,0]]]]"
   , "[[[[8,1],5],0],[[[3,9],[8,3]],[[6,9],[5,1]]]]"
   , "[[7,7],[[[8,5],2],[9,2]]]"
   , "[[[[4,9],9],[6,[5,3]]],[[[7,1],[7,1]],[[9,5],[7,0]]]]"
   , "[[7,[0,5]],[7,[2,[1,6]]]]"
   , "[[9,[0,[0,2]]],[[1,1],[[6,6],[5,3]]]]"
   , "[[[2,9],[[6,9],9]],[[[4,2],7],[1,[2,3]]]]"
   , "[[[0,1],[3,3]],[3,[[2,7],2]]]"
   , "[[[5,6],8],[[[4,9],[3,3]],[6,[5,2]]]]"
   , "[[4,[4,[2,5]]],[[2,[4,8]],[3,[7,7]]]]"
   , "[[2,5],[[[9,6],[9,3]],[[4,5],[2,3]]]]"
   , "[[5,[0,5]],[[[2,1],[0,5]],3]]"
   , "[[[[2,0],5],[[7,9],[4,5]]],[0,[[1,4],9]]]"
   , "[[[[1,3],2],[[3,9],[9,5]]],[[[4,1],[3,8]],0]]"
   , "[[[[1,8],[8,3]],[3,0]],[[5,1],[4,8]]]"
   , "[[1,6],[3,2]]"
   , "[[4,5],[[[9,3],[8,6]],[2,[8,6]]]]"
   , "[[[[4,4],1],[[7,3],2]],[[9,[2,1]],[8,2]]]"
   , "[0,[[2,[3,8]],9]]"
   , "[[1,[5,0]],[0,[[2,6],[8,5]]]]"
   , "[[6,[6,1]],[[2,[7,9]],[[8,3],1]]]"
   , "[[[2,[5,9]],[[8,9],1]],[[[5,2],2],4]]"
   , "[[[4,3],5],[[6,[3,6]],5]]"
   , "[1,[6,[6,2]]]"
   , "[[[[4,9],3],9],[[3,9],[8,[4,9]]]]"
   , "[[[[7,1],[1,6]],[[7,8],[3,7]]],[[[5,3],7],[9,[3,1]]]]"
   , "[[[[0,8],[8,9]],2],7]"
   , "[[[[3,7],[9,8]],[[7,1],8]],[[4,[4,6]],8]]"
   , "[3,[3,[[4,4],5]]]"
   , "[[3,[[2,3],7]],[[7,9],2]]"
   , "[[[[0,6],[5,1]],[[7,2],5]],[9,8]]"
   , "[[4,0],[[4,3],[7,2]]]"
   , "[[[8,[1,1]],[7,[9,1]]],[9,[9,[0,8]]]]"
   , "[9,[[[4,5],8],[[3,4],9]]]"
   , "[[[6,[4,7]],[8,7]],[[[3,8],5],[[2,1],[3,5]]]]"
   , "[[[[5,5],[6,8]],[[2,3],6]],[8,[5,7]]]"
   , "[[5,[[6,1],[3,6]]],[[[0,6],[7,1]],[9,[8,4]]]]"
   , "[[[[0,1],[4,9]],[[1,7],[3,3]]],[6,[3,[6,1]]]]"
   , "[[[[3,8],5],[[4,7],2]],2]"
   , "[[6,[[4,4],0]],[[2,[4,5]],[8,2]]]"
   , "[[6,[9,[7,0]]],[[9,[1,6]],[[6,1],1]]]"
   , "[[[[2,1],[5,7]],[5,[9,3]]],[[[7,9],[4,2]],4]]"
   , "[[3,1],[[7,8],[[8,8],9]]]"
   , "[[[[9,4],[1,8]],[9,[3,7]]],[[6,9],[[7,2],1]]]"
   , "[[[9,3],2],9]" ]
