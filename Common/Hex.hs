module AdventOfCode.Common.Hex where

hexToBCD :: Char -> String
hexToBCD '0' = "0000"
hexToBCD '1' = "0001"
hexToBCD '2' = "0010"
hexToBCD '3' = "0011"
hexToBCD '4' = "0100"
hexToBCD '5' = "0101"
hexToBCD '6' = "0110"
hexToBCD '7' = "0111"
hexToBCD '8' = "1000"
hexToBCD '9' = "1001"
hexToBCD 'A' = "1010"
hexToBCD 'B' = "1011"
hexToBCD 'C' = "1100"
hexToBCD 'D' = "1101"
hexToBCD 'E' = "1110"
hexToBCD 'F' = "1111"