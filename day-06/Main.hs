import Data.List (foldl', nub)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  input <- Text.readFile "input.txt"
  let fish = read . Text.unpack <$> Text.splitOn "," (Text.strip input)
  print $ part1 fish
  print $ part2 fish
  return ()

part1 :: [Int] -> Int
part1 = solve 80

part2 :: [Int] -> Int
part2 = solve 256

solve :: Int -> [Int] -> Int
solve n fish = sum $ f n <$> count fish

count :: Ord a => [a] -> [(a, Int)]
count xs = map (\x -> (x, length $ filter (== x) xs)) $ nub xs

f :: Int -> (Int, Int) -> Int
f days (start, numFish) = numFish + sum [f d (8, numFish) | d <- [days - start - 1, days - start - 8 .. 0]]