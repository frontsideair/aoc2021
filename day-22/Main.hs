import Data.Functor (($>))
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (char, digit, endBy1, endOfLine, eof, many1, space, string, try, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

type Range = (Int, Int)

type Coord = (Int, Int, Int)

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ length $ foldl' f Set.empty input

f :: Set Coord -> (Bool, (Range, Range, Range)) -> Set Coord
f acc (True, ranges) = Set.union acc $ Set.fromList $ coords ranges
f acc (False, ranges) = Set.difference acc $ Set.fromList $ coords ranges

coords :: (Range, Range, Range) -> [Coord]
coords (xRange, yRange, zRange) = [(x, y, z) | x <- range xRange, y <- range yRange, z <- range zRange]
  where
    range (start, end) = [(max (-50) start) .. (min 50 end)]

parser :: Parser [(Bool, (Range, Range, Range))]
parser = lineParser `endBy1` endOfLine <* eof

lineParser :: Parser (Bool, (Range, Range, Range))
lineParser = do
  on <- onOff
  space
  string "x="
  x <- rangeParser
  string ","
  string "y="
  y <- rangeParser
  string ","
  string "z="
  z <- rangeParser
  return (on, (x, y, z))

onOff :: Parser Bool
onOff = try (string "on" $> True) <|> try (string "off" $> False)

rangeParser :: Parser Range
rangeParser = do
  start <- int
  string ".."
  end <- int
  return (start, end)

int :: Parser Int
int = do
  sign <- (char '-' >> return negate) <|> return id
  num <- many1 digit
  return $ sign $ read num