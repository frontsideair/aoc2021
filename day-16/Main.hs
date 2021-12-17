import Control.Monad (when)
import Data.Functor (($>))
import Text.Parsec (char, choice, count, endOfLine, eof, many1, parse, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

data Packet = Packet Int Packet' deriving (Show)

data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | Equal deriving (Show)

data Packet' = Literal Int | Operator Op [Packet] deriving (Show)

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let Right result = parse packetParser "" input
  print $ part1 result
  print $ part2 result

part1 :: Packet -> Int
part1 = sumVersions

part2 :: Packet -> Int
part2 = eval

sumVersions :: Packet -> Int
sumVersions (Packet n (Literal _)) = n
sumVersions (Packet n (Operator _ ops)) = n + sum (sumVersions <$> ops)

eval :: Packet -> Int
eval (Packet _ (Literal v)) = v
eval (Packet _ (Operator op packets)) =
  case op of
    Sum -> sum (eval <$> packets)
    Product -> product (eval <$> packets)
    Minimum -> minimum (eval <$> packets)
    Maximum -> maximum (eval <$> packets)
    GreaterThan -> let [l, r] = packets in if eval l > eval r then 1 else 0
    LessThan -> let [l, r] = packets in if eval l < eval r then 1 else 0
    Equal -> let [l, r] = packets in if eval l == eval r then 1 else 0

parser :: Parser String
parser = concat <$> many1 hexParser <* endOfLine <* eof
  where
    hexParser =
      choice
        [ char '0' $> "0000",
          char '1' $> "0001",
          char '2' $> "0010",
          char '3' $> "0011",
          char '4' $> "0100",
          char '5' $> "0101",
          char '6' $> "0110",
          char '7' $> "0111",
          char '8' $> "1000",
          char '9' $> "1001",
          char 'A' $> "1010",
          char 'B' $> "1011",
          char 'C' $> "1100",
          char 'D' $> "1101",
          char 'E' $> "1110",
          char 'F' $> "1111"
        ]

packetParser :: Parser Packet
packetParser = do
  version <- bits 3
  typeId <- bits 3
  Packet version <$> if typeId == 4 then Literal . binaryToInteger <$> literalParser else Operator (intToOp typeId) <$> operatorParser

intToOp :: Int -> Op
intToOp 0 = Sum
intToOp 1 = Product
intToOp 2 = Minimum
intToOp 3 = Maximum
intToOp 5 = GreaterThan
intToOp 6 = LessThan
intToOp 7 = Equal
intToOp _ = error "Invalid op"

literalParser :: Parser String
literalParser = do
  start <- bit
  chunk <- count 4 bit
  if start == '1' then (chunk ++) <$> literalParser else return chunk

-- operatorParser :: Parser Packet'
operatorParser = do
  lengthTypeId <- bit
  if lengthTypeId == '0' then bits 15 >>= operatorLength else bits 11 >>= operatorCount

-- operatorCount :: Int -> Parser Packet'
operatorCount n = count n packetParser

-- operatorLength :: Int -> Parser Packet'
operatorLength n = do
  text <- count n bit
  return $ either (error . show) id $ parse (many1 packetParser) "" text

bits :: Int -> Parser Int
bits n = binaryToInteger <$> count n bit

bit :: Parser Char
bit = char '0' <|> char '1'

binaryToInteger :: [Char] -> Int
binaryToInteger = foldl (\acc x -> acc * 2 + (if x == '1' then 1 else 0)) 0
