{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Y2021.Day16 where

import AdventOfCode.Common.Parser
import AdventOfCode.Common.Binary
import AdventOfCode.Common.Hex
import AdventOfCode.Common.Util

import Control.Monad

--------------------------------------------------------------------------------

data Operator = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo
   deriving (Show, Eq, Enum, Bounded)

data PacketValue = Literal Int | Operation Operator [Packet]
   deriving (Show)

data Packet = Packet
   { version :: Int
   , value :: PacketValue
   } deriving (Show)

--------------------------------------------------------------------------------

getSubPackets :: Packet -> [Packet]
getSubPackets p = case value p of
   Literal _ -> []
   Operation _ ps -> ps

sumVersionNumbers :: Packet -> Int
sumVersionNumbers p = version p + sum (map sumVersionNumbers (getSubPackets p))

buildPacket :: String -> Packet
buildPacket input = parseUsing packetParser (input >>= hexToBCD)

part1 :: String -> Int
part1 = sumVersionNumbers . buildPacket

--------------------------------------------------------------------------------

eval :: Packet -> Int
eval (Packet _ (Literal n)) = n
eval (Packet _ (Operation op ps)) = evalOp op $ map eval ps

evalOp :: Operator -> [Int] -> Int
evalOp Sum = sum
evalOp Product = product
evalOp Minimum = minimum
evalOp Maximum = maximum
evalOp GreaterThan = \[a,b] -> if a > b then 1 else 0
evalOp LessThan = \[a,b] -> if a < b then 1 else 0
evalOp EqualTo = \[a,b] -> if a == b then 1 else 0

part2 :: String -> Int
part2 = eval . buildPacket

--------------------------------------------------------------------------------

sampleInput1 = "8A004A801A8002F478"
sampleInput2 = "620080001611562C8802118E34"
sampleInput3 = "C0015000016115A2E0802F182340"
sampleInput4 = "A0016C880162017C3686B18A3D4780"

myInput :: String
myInput = "E20D79005573F71DA0054E48527EF97D3004653BB1FC006867A8B1371AC49C801039171941340066E6B99A6A58B8110088BA008CE6F7893D4E6F7893DCDCFDB9D6CBC4026FE8026200DC7D84B1C00010A89507E3CCEE37B592014D3C01491B6697A83CB4F59E5E7FFA5CC66D4BC6F05D3004E6BB742B004E7E6B3375A46CF91D8C027911797589E17920F4009BE72DA8D2E4523DCEE86A8018C4AD3C7F2D2D02C5B9FF53366E3004658DB0012A963891D168801D08480485B005C0010A883116308002171AA24C679E0394EB898023331E60AB401294D98CA6CD8C01D9B349E0A99363003E655D40289CBDBB2F55D25E53ECAF14D9ABBB4CC726F038C011B0044401987D0BE0C00021B04E2546499DE824C015B004A7755B570013F2DD8627C65C02186F2996E9CCD04E5718C5CBCC016B004A4F61B27B0D9B8633F9344D57B0C1D3805537ADFA21F231C6EC9F3D3089FF7CD25E5941200C96801F191C77091238EE13A704A7CCC802B3B00567F192296259ABD9C400282915B9F6E98879823046C0010C626C966A19351EE27DE86C8E6968F2BE3D2008EE540FC01196989CD9410055725480D60025737BA1547D700727B9A89B444971830070401F8D70BA3B8803F16A3FC2D00043621C3B8A733C8BD880212BCDEE9D34929164D5CB08032594E5E1D25C0055E5B771E966783240220CD19E802E200F4588450BC401A8FB14E0A1805B36F3243B2833247536B70BDC00A60348880C7730039400B402A91009F650028C00E2020918077610021C00C1002D80512601188803B4000C148025010036727EE5AD6B445CC011E00B825E14F4BBF5F97853D2EFD6256F8FFE9F3B001420C01A88915E259002191EE2F4392004323E44A8B4C0069CEF34D304C001AB94379D149BD904507004A6D466B618402477802E200D47383719C0010F8A507A294CC9C90024A967C9995EE2933BA840"

--------------------------------------------------------------------------------

packetParser :: Parser Packet
packetParser = do
   p <- subPacketParser
   trailingZeroes <- greedy string
   guard $ all (== '0') trailingZeroes
   return p

subPacketParser :: Parser Packet
subPacketParser = do
   (version, typeID) <- headerParser
   x <- pickValueParser typeID
   return $ Packet version x

headerParser :: Parser (Int,Int)
headerParser = do
   v <- chars 3
   t <- chars 3
   return (readBin v, readBin t)

pickValueParser :: Int -> Parser PacketValue
pickValueParser = \case
   4 -> literalParser
   n -> operatorParser n

literalParser :: Parser PacketValue
literalParser = do
   x <- aux
   return $ Literal $ readBin x
   where
   aux :: Parser String
   aux = chars 5 >>= \(l:b) -> if l == '0' then return b else fmap (b++) aux

operatorParser :: Int -> Parser PacketValue
operatorParser typeID = do
   lengthTypeID <- char
   subPackets <- if lengthTypeID == '0' then operator0Parser else operator1Parser
   return $ Operation (getOperator typeID) subPackets

operator0Parser :: Parser [Packet]
operator0Parser = do
   subPacketsTotalLength <- readBin <$> chars 15
   subPacketBits <- chars subPacketsTotalLength
   parseM (many subPacketParser) subPacketBits

operator1Parser :: Parser [Packet]
operator1Parser = do
   numberOfSubPackets <- readBin <$> chars 11
   replicateP numberOfSubPackets subPacketParser

getOperator :: Int -> Operator
getOperator = \case
   0 -> Sum
   1 -> Product
   2 -> Minimum
   3 -> Maximum
   5 -> GreaterThan
   6 -> LessThan
   7 -> EqualTo
