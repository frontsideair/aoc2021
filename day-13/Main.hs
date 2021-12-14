import Data.Foldable (Foldable (foldl', toList))
import Data.Functor (($>))
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Text.Parsec (char, digit, endBy1, endOfLine, eof, many1, string, (<|>))
import Text.Parsec.Text (Parser, parseFromFile)

data Axis = X | Y deriving (Eq, Show)

newtype Cell = Cell Char deriving (Eq)

instance Semigroup Cell where
  Cell '#' <> Cell _ = Cell '#'
  Cell _ <> Cell '#' = Cell '#'
  _ <> _ = Cell '.'

instance Show Cell where
  show (Cell '#') = "#"
  show _ = "."

main :: IO ()
main = do
  (coordinates, instructions) <- parseFromFile parser "input.txt" >>= either (error . show) return
  let (xs, ys) = unzip coordinates
  let xsize = maximum xs + 1
  let ysize = maximum ys + 1
  let coordinates' = Set.fromList coordinates
  let matrix = Matrix.matrix ysize xsize (\(x, y) -> if (y - 1, x - 1) `Set.member` coordinates' then Cell '#' else Cell '.')
  print $ part1 matrix instructions
  print $ part2 matrix instructions

part1 :: Matrix Cell -> [(Axis, Int)] -> Int
part1 matrix instructions = countDots $ foldMatrix matrix (head instructions)

part2 :: Matrix Cell -> [(Axis, Int)] -> Matrix Cell
part2 = foldl' foldMatrix

countDots :: Matrix Cell -> Int
countDots matrix = length $ filter (== Cell '#') $ toList matrix

foldMatrix :: Matrix Cell -> (Axis, Int) -> Matrix Cell
foldMatrix matrix (X, index) = combine X left right
  where
    left = Matrix.submatrix 1 (Matrix.nrows matrix) 1 index matrix
    right = Matrix.forceMatrix $ Matrix.submatrix 1 (Matrix.nrows matrix) (index + 2) (Matrix.ncols matrix) matrix
foldMatrix matrix (Y, index) = combine Y top bottom
  where
    top = Matrix.submatrix 1 index 1 (Matrix.ncols matrix) matrix
    bottom = Matrix.forceMatrix $ Matrix.submatrix (index + 2) (Matrix.nrows matrix) 1 (Matrix.ncols matrix) matrix

flipMatrix :: Axis -> Matrix a -> Matrix a
flipMatrix X matrix = foldl' (\m index -> Matrix.switchCols index (ncols - index + 1) m) matrix [1 .. (ncols `div` 2)]
  where
    ncols = Matrix.ncols matrix
flipMatrix Y matrix = foldl' (\m index -> Matrix.switchRows index (nrows - index + 1) m) matrix [1 .. nrows `div` 2]
  where
    nrows = Matrix.nrows matrix

combine :: Axis -> Matrix Cell -> Matrix Cell -> Matrix Cell
combine X left right = Matrix.elementwise (<>) left (fill X (Matrix.ncols left) (flipMatrix X right))
combine Y top bottom = Matrix.elementwise (<>) top (fill Y (Matrix.nrows top) (flipMatrix Y bottom))

fill :: Axis -> Int -> Matrix Cell -> Matrix Cell
fill X size matrix = (Matrix.<|>) (false (Matrix.nrows matrix) (size - Matrix.ncols matrix)) matrix
fill Y size matrix = (Matrix.<->) (false (size - Matrix.nrows matrix) (Matrix.ncols matrix)) matrix

false :: Int -> Int -> Matrix Cell
false rows cols = Matrix.matrix rows cols (const $ Cell '.')

parser :: Parser ([(Int, Int)], [(Axis, Int)])
parser = do
  coordinates <- coordinate `endBy1` endOfLine
  endOfLine
  instructions <- instruction `endBy1` endOfLine
  eof
  return (coordinates, instructions)

coordinate :: Parser (Int, Int)
coordinate = do
  x <- number
  char ','
  y <- number
  return (x, y)

instruction :: Parser (Axis, Int)
instruction = do
  string "fold along "
  axis <- axisParser
  char '='
  value <- number
  return (axis, value)

axisParser :: Parser Axis
axisParser = char 'x' $> X <|> char 'y' $> Y

number :: Parser Int
number = read <$> many1 digit