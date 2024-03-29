module AdventOfCode.Common.Binary where

binToDec :: Num a => [Bool] -> a
binToDec = foldr (\x y -> (if x then 1 else 0) + 2 * y) 0

readBin :: Num a => String -> a
readBin = binToDec . reverse . map (=='1')

invertBin :: String -> String
invertBin = map $ \b -> if b == '0' then '1' else '0'

showBin :: (Show a, Integral a) => a -> String
showBin 0 = "0"
showBin n = let (d,m) = n `divMod` 2 in showNonZero d ++ show m
   where
   showNonZero 0 = ""
   showNonZero n = showBin n
