import Control.Monad (join)
import Text.Parsec (char, count, endBy, endOfLine, eof, many, many1, oneOf, sepBy, string, try)
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

part1 :: [[String]] -> Int
part1 outputs = length $ filter (\x -> length x `elem` uniques) (join outputs)
  where
    uniques = [length $ digits !! i | i <- [1, 4, 7, 8]]

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