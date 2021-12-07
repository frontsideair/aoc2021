import Data.Foldable (Foldable (foldl'))
import Data.List (tails)

f :: [Int] -> (Int, Integer)
f = foldl' (\(last, n) measurement -> (measurement, if measurement > last then n + 1 else n)) (maxBound, 0)

-- take first n elements for each element
aperture :: Int -> [a] -> [[a]]
aperture n xs = map (take n) (tails xs)

part1 :: IO ()
part1 = do
  file <- readFile "input.txt"
  let measurements :: [Int] = read <$> lines file
  let (_, n) = f measurements
  print n

part2 :: IO ()
part2 = do
  file <- readFile "input.txt"
  let measurements :: [Int] = read <$> lines file
  let chunks = sum <$> aperture 3 measurements
  let (_, n) = f chunks
  print n

main :: IO ()
main = do
  part1
  part2