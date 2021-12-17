import Data.Foldable (foldl')
import Data.Heap (Entry (Entry), Heap)
import qualified Data.Heap as Heap
import Data.List (foldl1')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Matrix (Matrix, (!), (<->), (<|>))
import qualified Data.Matrix as Matrix
import Text.Parsec (count, digit, endBy1, endOfLine, eof, many1)
import Text.Parsec.Text (Parser, parseFromFile)

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ part1 input
  print $ part2 input

part1 :: Matrix Int -> Maybe Int
part1 input = Map.lookup (Matrix.nrows input, Matrix.ncols input) $ shortestPath input

part2 :: Matrix Int -> Maybe Int
part2 = part1 . enlarge

enlarge :: Matrix Int -> Matrix Int
enlarge = go (<->) . go (<|>)
  where
    go f input = foldl1' f (take 5 $ iterate (fmap inc) input)
    inc n = if n + 1 > 9 then 1 else n + 1

shortestPath :: Matrix Int -> Map (Int, Int) Int
shortestPath matrix = go matrix knownDistances visited
  where
    knownDistances = Heap.singleton $ Entry 0 (1, 1)
    visited = Map.empty
    go matrix knownDistances visited = case Heap.uncons knownDistances of
      Nothing -> visited
      Just (Entry distV v, knownDistances') -> go matrix knownDistances'' visited'
        where
          ns = filter (`Map.notMember` visited) (neighbors v (Matrix.nrows matrix) (Matrix.ncols matrix))
          dists = (+ distV) . (matrix !) <$> ns
          knownDistances'' = foldl' (updatePriority min) knownDistances' (zipWith Entry dists ns)
          visited' = Map.insert v distV visited

updatePriority :: (Ord p, Eq a) => (p -> p -> p) -> Heap (Entry p a) -> Entry p a -> Heap (Entry p a)
updatePriority f heap (Entry a b) = Heap.insert new tail
  where
    (head, tail) = Heap.partition (\(Entry _ b') -> b == b') heap
    Entry old _ = Heap.minimum head
    new = if Heap.null head then Entry a b else Entry (f old a) b

neighbors :: (Int, Int) -> Int -> Int -> [(Int, Int)]
neighbors (y, x) yMax xMax = filter (\(y', x') -> x' > 0 && x' <= xMax && y' > 0 && y' <= yMax) [(y + 1, x), (y -1, x), (y, x + 1), (y, x - 1)]

parser :: Parser (Matrix Int)
parser = Matrix.fromLists <$> lineParser `endBy1` endOfLine <* eof

lineParser :: Parser [Int]
lineParser = many1 numberParser

numberParser :: Parser Int
numberParser = read <$> count 1 digit