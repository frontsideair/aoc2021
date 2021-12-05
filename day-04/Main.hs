import Data.List (find, inits, partition, (\\))
import Data.Matrix (Matrix, fromLists, toList, toLists, transpose)
import Data.Maybe (catMaybes)
import Data.Set (fromList)
import Text.Parsec (char, count, digit, endBy, endOfLine, eof, many, sepBy, space)
import Text.Parsec.Text (Parser, parseFromFile)

main :: IO ()
main = do
  parseResult <- parseFromFile parser "input.txt"
  let (numbers, boards) = case parseResult of
        Left err -> error $ show err
        Right input -> input
  let steps = [(partition isWinner $ punch sofar <$> boards, last sofar) | sofar <- inits numbers]
  print $ result $ part1 steps
  print $ result $ part2 steps

part1 steps = let Just (([board], _), lastNumber) = find (\((winningBoards, losingBoards), lastNumber) -> length winningBoards == 1) steps in (board, lastNumber)

part2 steps =
  let Just ((_, [losingBoard]), _) = find (\((winningBoards, losingBoards), lastNumber) -> length losingBoards == 1) steps
      Just ((_, _), lastNumber) = find (\((winningBoards, losingBoards), lastNumber) -> null losingBoards) steps
   in ((\n -> if Just lastNumber == n then Nothing else n) <$> losingBoard, lastNumber)

result :: (Matrix (Maybe Int), Int) -> Int
result (board, lastNumber) = lastNumber * sum (catMaybes $ toList board)

isWinner :: Matrix (Maybe Int) -> Bool
isWinner board = isVerticalWinner || isHorizontalWinner
  where
    isVerticalWinner = elem bingo $ toLists board
    isHorizontalWinner = elem bingo $ toLists $ transpose board
    bingo = replicate 5 Nothing

punch :: [Int] -> Matrix Int -> Matrix (Maybe Int)
punch numbers board = f <$> board
  where
    f n = if n `elem` numbers then Nothing else Just n

parser :: Parser ([Int], [Matrix Int])
parser = do
  numbers <- numbersParser
  many endOfLine
  boards <- boardParser `sepBy` endOfLine
  eof
  return (numbers, boards)

-- parse numbers separated by comma
numbersParser :: Parser [Int]
numbersParser = num `sepBy` char ','

num :: Parser Int
num = read <$> many digit

-- parse boards
boardParser :: Parser (Matrix Int)
boardParser =
  fromLists <$> count 5 (lineParser <* endOfLine)

lineParser :: Parser [Int]
lineParser = count 5 $ many space *> num
