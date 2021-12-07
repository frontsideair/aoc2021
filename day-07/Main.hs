import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  input <- Text.readFile "input.txt"
  let crabs = read . Text.unpack <$> Text.splitOn "," (Text.strip input)
  print $ part1 crabs
  print $ part2 crabs
  return ()

part1 :: [Int] -> Int
part1 crabs = minimum $ distance1 crabs <$> [minimum crabs .. maximum crabs]

part2 :: [Int] -> Int
part2 crabs = minimum $ distance2 crabs <$> [minimum crabs .. maximum crabs]

distance1 :: [Int] -> Int -> Int
distance1 crabs target = sum $ abs . (target -) <$> crabs

distance2 :: [Int] -> Int -> Int
distance2 crabs target = sum $ sumUpTo . abs . (target -) <$> crabs

sumUpTo :: Int -> Int
sumUpTo n = sum [1 .. n]