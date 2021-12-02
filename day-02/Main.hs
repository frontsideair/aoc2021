import Data.Either (fromRight)
import Data.Foldable (foldl')
import Data.Text (Text)
import Text.Parsec (digit, endOfLine, eof, many, space, string, (<|>))
import Text.Parsec.Text (Parser, parseFromFile)

data Dir = Forward Int | Up Int | Down Int deriving (Eq, Show)

main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  parseResult <- parseFromFile parser "input.txt"
  let parsed = fromRight (error "Could not parse input") parseResult
  let (x, y) =
        foldl'
          ( \(x, y) dir -> case dir of
              Forward n -> (x + n, y)
              Up n -> (x, y - n)
              Down n -> (x, y + n)
          )
          (0, 0)
          parsed
  print (x * y)

part2 :: IO ()
part2 = do
  parseResult <- parseFromFile parser "input.txt"
  let parsed = fromRight (error "Could not parse input") parseResult
  let (x, y, aim) =
        foldl'
          ( \(x, y, aim) dir -> case dir of
              Forward n -> (x + n, y + aim * n, aim)
              Up n -> (x, y, aim - n)
              Down n -> (x, y, aim + n)
          )
          (0, 0, 0)
          parsed
  print (x * y)

parser :: Parser [Dir]
parser = do
  result <- many line
  eof
  return result

line :: Parser Dir
line = do
  dir <- f "forward" Forward <|> f "up" Up <|> f "down" Down
  endOfLine
  return dir

f :: String -> (Int -> Dir) -> Parser Dir
f dir a = do
  string dir
  space
  a <$> num

num :: Parser Int
num = read <$> many digit