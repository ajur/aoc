{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Advent Of Code 2021 Day 16
-- https://adventofcode.com/2021/day/16

-- solution by Adam Jurczyk
-- https://github.com/ajur/aoc

import Tools

import Text.ParserCombinators.ReadP
import Control.Applicative

main :: IO ()
main = do
    inputData <- readInput sampleData
    
    let packets = parseInput inputData
    
    putStrLn "--- input packets"
    putStrLn . show $ packets
    
    putStrLn "--- part 1"
    putStrLn . show . sumVersions $ packets

    putStrLn "--- part 2"
    putStrLn . show . packetEval $ packets



type Version = Int

data OpType = OpSum | OpProd| OpMin | OpMax | OpGt | OpLt | OpEq deriving (Show)

data Packet = Literal Version Int
            | Operator Version OpType [Packet]

instance Show Packet where
    show = pprintPacket

isLiteralKind :: Int -> Bool
isLiteralKind = (==4)

kindToOperatorType :: Int -> OpType
kindToOperatorType 0 = OpSum  
kindToOperatorType 1 = OpProd 
kindToOperatorType 2 = OpMin  
kindToOperatorType 3 = OpMax  
kindToOperatorType 5 = OpGt   
kindToOperatorType 6 = OpLt   
kindToOperatorType 7 = OpEq   

packetVersion :: Packet -> Version
packetVersion (Literal v _)  = v
packetVersion (Operator v _ _) = v

packetSubPackets :: Packet -> [Packet]
packetSubPackets (Literal _ _) = []
packetSubPackets (Operator _ _ ps) = ps

packetEval :: Packet -> Int
packetEval (Literal _ v) = v
packetEval (Operator _ OpSum ps) = sum . map packetEval $ ps
packetEval (Operator _ OpProd ps) = product . map packetEval $ ps
packetEval (Operator _ OpMin ps) = minimum . map packetEval $ ps
packetEval (Operator _ OpMax ps) = maximum . map packetEval $ ps
packetEval (Operator _ OpGt [l,p]) = if packetEval l > packetEval p then 1 else 0
packetEval (Operator _ OpLt [l,p]) = if packetEval l < packetEval p then 1 else 0
packetEval (Operator _ OpEq [l,p]) = if packetEval l == packetEval p then 1 else 0

sumVersions :: Packet -> Int
sumVersions p = packetVersion p + sum (map sumVersions (packetSubPackets p))

pprintPacket :: Packet -> String
pprintPacket = intercalate "\n". ppp
    where
        ppp (Literal v i)     = ["<Literal version=" ++ show v ++ " value=" ++ show i ++ ">"]
        ppp (Operator v t ps) = ("<" ++ show t ++ " " ++ "version=" ++ show v ++ ">") : (map ("  "++) . concatMap ppp $ ps)


readPacket :: ReadP Packet
readPacket = readLiteral <|> readOperator

readLiteral :: ReadP Packet
readLiteral = do
    version <- readBits 3
    kind <- readBits 3

    if not (isLiteralKind kind) then
        pfail
    else do
        value <- readLiteralValue
        return (Literal version value)

readOperator :: ReadP Packet
readOperator = do
    version <- readBits 3
    kind <- readBits 3

    if isLiteralKind kind then
        pfail
    else do
        subs <- readOperatorSubPacketsByLength <|> readOperatorSubPacketsByNumber
        return (Operator version (kindToOperatorType kind) subs)

readOperatorSubPacketsByLength :: ReadP [Packet]
readOperatorSubPacketsByLength = do
    satisfy (=='0')
    packetLength <- readBits 15
    subPackets <- readAtMostNCharsWith packetLength readPacket
    return subPackets

readOperatorSubPacketsByNumber :: ReadP [Packet]
readOperatorSubPacketsByNumber = do
    satisfy (=='1')
    packetsCount <- readBits 11
    subPackets <- count packetsCount readPacket
    return subPackets

readLiteralValue :: ReadP Int
readLiteralValue = fmap bs2i . fmap concat $ manyTillIncluding readLiteralValuePart readLiteralValuePartEnd

readLiteralValuePart :: ReadP String
readLiteralValuePart = do
    groupEnd <- satisfy (=='1')
    val <- count 4 get
    return val
readLiteralValuePartEnd :: ReadP String
readLiteralValuePartEnd = do
    groupEnd <- satisfy (=='0')
    val <- count 4 get
    return val

readAtMostNCharsWith :: Int -> ReadP a -> ReadP [a]
readAtMostNCharsWith n p = do
    if n <= 0 then
        return []
    else do
        (s, v) <- gather p
        vs <- readAtMostNCharsWith (n - length s) p
        return (v:vs)


manyTillIncluding :: ReadP a -> ReadP a -> ReadP [a]  -- veration over manyTill, that includes end
manyTillIncluding p end = scan
  where
      scan = (fmap (:[]) end) <++ (liftM2 (:) p scan)
      liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }  -- im not exactly sure what it does, just copied it as is xD

readBits :: Int -> ReadP Int
readBits n = fmap bs2i $ count n get

-- it was already there so...
x2b :: Char -> String
x2b '0' = "0000"
x2b '1' = "0001"
x2b '2' = "0010"
x2b '3' = "0011"
x2b '4' = "0100"
x2b '5' = "0101"
x2b '6' = "0110"
x2b '7' = "0111"
x2b '8' = "1000"
x2b '9' = "1001"
x2b 'A' = "1010"
x2b 'B' = "1011"
x2b 'C' = "1100"
x2b 'D' = "1101"
x2b 'E' = "1110"
x2b 'F' = "1111"

parseInput :: String -> Packet
parseInput input = result
    where
        bits = concat . map x2b
        ((result, _):_) = readP_to_S readPacket $ bits input


sampleData :: String
sampleData = "A0016C880162017C3686B18A3D4780"

samples = [
    "D2FE28",
    "38006F45291200",
    "EE00D40C823060",
    "8A004A801A8002F478",
    "620080001611562C8802118E34",
    "C0015000016115A2E0802F182340",
    "A0016C880162017C3686B18A3D4780"]

{-

D2FE28
110100101111111000101000
VVVIIIb----b----B----
  6  4 0111 1110 0101
-> v 6, id 4 (num), 2021

38006F45291200
00111000000000000110111101000101001010010001001000000000
VVVIIILbbbbbbbbbbbbbbb---------------------------
  1  6f               VVVIIIB----VVVIIIb----B----
                        6  4       2  4        20
-> v 1, id 6 (op), Ltype 0 -> 15 bits = 27, so 27 bits of subpackets
                      -> v 6, id 4 (num), 10
                      -> v 2, id 4 (num), 20

EE00D40C823060
11101110000000001101010000001100100000100011000001100000
VVVIIILbbbbbbbbbbb
  7  3c           VVVIIIB----VVVIIIB----VVVIIIB----
                    2  4  (1)  4  4  (2)  1  4  (3)
-> v 7, id 3 (op), Ltype 1 -> 11 bits = 3, so 3 subpackets

8A004A801A8002F478   lv 16
100010100000000001001010100000000001101010000000000000101111010001111000
VVVIIIL-----------
  4  2c       (1) VVVIIIL-----------
                    1  2c        (1)VVVIIIL---------------###########
                                      5  2f          (11) VVVIIIB----
                                                            6  4   15
620080001611562C8802118E34    lv 12
01100010000000001000000000000000000101100001000101010110001011001000100000000010000100011000111000110100

C0015000016115A2E0802F182340     lv 23
1100000000000001010100000000000000000001011000010001010110100010111000001000000000101111000110000010001101000000

A0016C880162017C3686B18A3D4780     lv 31
101000000000000101101100100010000000000101100010000000010111110000110110100001101011000110001010001111010100011110000000

-}
