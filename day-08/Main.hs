import Control.Monad (join)
import Data.List (find, findIndex, union, (\\))
import Data.Maybe (fromJust)
import Text.Parsec (char, endBy, endOfLine, eof, many1, oneOf, sepBy, string)
import Text.Parsec.String (Parser, parseFromFile)

digits :: [[Char]]
digits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

main :: IO ()
main = do
  parseResult <- parseFromFile parser "input.txt"
  let result = case parseResult of
        Left err -> error $ show err
        Right x -> x
  print $ part1 $ snd <$> result
  print $ part2 result

part1 :: [[String]] -> Int
part1 outputs = length $ filter (\x -> length x `elem` uniques) (join outputs)
  where
    uniques = [length $ digits !! i | i <- [1, 4, 7, 8]]

part2 :: [([String], [String])] -> Int
part2 lines = sum $ findOutputs <$> lines
  where
    findOutputs (signalPatterns, output) = fixOutput output $ findPattern signalPatterns
    fixOutput output segments = toDecimal (fixDigit segments <$> output)
    fixDigit segments digit = fromJust $ findIndex (\x -> x \\ digit == digit \\ x) segments

-- find rules to determine digit segments
-- 2 segment = 1
-- 4 segment = 4
-- 3 segment = 7
-- 7 segment = 8
-- 5 segment = 3 | 2 segment \\ 5 segment == null
-- 5 segment = 2 | 5 segment `union` 4 segment == 7 segment
-- 5 segment = 5 | otherwise
-- 6 segment = 6 | 2 segment \\ 6 segment != null
-- 6 segment = 9 | 6 segment `union` 4 segment != 7 segment
-- 6 segment = 0 | otherwise

findPattern :: [[Char]] -> [[Char]]
findPattern signalPatterns = [zero, one, two, three, four, five, six, seven, eight, nine]
  where
    Just one = find (\x -> length x == 2) signalPatterns
    Just four = find (\x -> length x == 4) signalPatterns
    Just seven = find (\x -> length x == 3) signalPatterns
    Just eight = find (\x -> length x == 7) signalPatterns
    Just three = find (\x -> (length x == 5) && null (one \\ x)) signalPatterns
    Just two = find (\x -> (length x == 5) && (length (x `union` four) == 7)) signalPatterns
    Just five = find (\x -> (length x == 5) && x `notElem` [two, three]) signalPatterns
    Just six = find (\x -> length x == 6 && (not . null) (one \\ x)) signalPatterns
    Just nine = find (\x -> length x == 6 && (length (x `union` four) /= 7)) signalPatterns
    Just zero = find (\x -> length x == 6 && x `notElem` [six, nine]) signalPatterns

toDecimal :: [Int] -> Int
toDecimal digits = read $ mconcat $ show <$> digits

parser :: Parser [([String], [String])]
parser = lineParser `endBy` endOfLine <* eof

lineParser :: Parser ([String], [String])
lineParser = do
  signalPatterns <- digitParser `endBy` char ' '
  string "| "
  output <- digitParser `sepBy` char ' '
  return (signalPatterns, output)

digitParser :: Parser String
digitParser = many1 $ oneOf "abcdefg"