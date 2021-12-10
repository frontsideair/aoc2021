import Data.List (sort)
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (count, digit, endBy1, endOfLine, eof, many1)
import Text.Parsec.Text (Parser, parseFromFile)

dimensions :: Matrix a -> (Int, Int)
dimensions m = (Matrix.nrows m, Matrix.ncols m)

neighbors :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbors (rows, cols) (x, y) = filter withinBounds [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    withinBounds (x, y) = within rows x && within cols y
    within size n = n >= 1 && n <= size

grow' :: [((Int, Int), Int)] -> Set (Int, Int) -> Matrix Int -> Set (Int, Int)
grow' current acc matrix = if null current then acc else grow' toAdd acc' matrix
  where
    toAdd = [(neighbor, neighborValue) | (k, v) <- current, neighbor <- neighbors (dimensions matrix) k, let neighborValue = matrix ! neighbor, neighborValue /= 9 && neighborValue > v]
    acc' = Set.union acc (Set.fromList $ fst <$> toAdd)

grow :: Matrix Int -> ((Int, Int), Int) -> Set (Int, Int)
grow matrix (k, v) = grow' [(k, v)] (Set.singleton k) matrix

main :: IO ()
main = do
  parseResult <- parseFromFile parser "input.txt"
  let input = case parseResult of
        Left err -> error $ show err
        Right input -> Matrix.fromLists input
  let output = Matrix.mapPos (\pos a -> if a < minimum ((input !) <$> neighbors (dimensions input) pos) then Just (pos, a) else Nothing) input
  let lows = catMaybes (Matrix.toList output)
  print $ part1 lows
  print $ part2 input lows

part1 :: [((Int, Int), Int)] -> Int
part1 lows = sum $ (+ 1) . snd <$> lows

part2 :: Matrix Int -> [((Int, Int), Int)] -> Int
part2 input lows = product $ take 3 $ reverse $ sort $ Set.size . grow input <$> lows

parser :: Parser [[Int]]
parser = line `endBy1` endOfLine <* eof

line :: Parser [Int]
line = many1 height

height :: Parser Int
height = read <$> count 1 digit