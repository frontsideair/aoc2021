import Data.List (foldl', partition, transpose)
import Text.Parsec (count, digit, endBy, endOfLine, eof, many1)
import Text.Parsec.Text (Parser, parseFromFile)

type Bits = [Int]

type Select = (Bits, Bits) -> Int

tally :: [Bits] -> Select -> Bits
tally bits select = selected
  where
    transposed = transpose bits
    partitioned = partition (0 ==) <$> transposed
    selected = select <$> partitioned

epsilon :: Select
epsilon (zeroes, ones) = if length zeroes > length ones then 0 else 1

gamma :: Select
gamma (zeroes, ones) = if length zeroes <= length ones then 0 else 1

tally2 :: Select -> Bits -> [Bits] -> Bits
tally2 select acc [bits] = reverse acc ++ bits
tally2 select acc bits = tally2 select (e : acc) filtered
  where
    e = head $ tally bits select
    filtered = tail <$> filter (\b -> head b == e) bits

oxygen :: [Bits] -> Bits
oxygen = tally2 epsilon []

co2 :: [Bits] -> Bits
co2 = tally2 gamma []

main :: IO ()
main = do
  parseResult <- parseFromFile parser "input.txt"
  let parsed = case parseResult of
        Left err -> error $ show err
        Right input -> input
  print $ day1 parsed
  print $ day2 parsed

day1 :: [Bits] -> Int
day1 parsed =
  let e = toDec $ tally parsed epsilon
      g = toDec $ tally parsed gamma
   in e * g

day2 :: [Bits] -> Int
day2 parsed =
  let o = toDec $ oxygen parsed
      c = toDec $ co2 parsed
   in o * c

parser :: Parser [Bits]
parser = do
  nums <- binaryParser `endBy` endOfLine
  eof
  return nums

binaryParser :: Parser Bits
binaryParser = many1 num

num :: Parser Int
num = read <$> count 1 digit

toDec :: [Int] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0
