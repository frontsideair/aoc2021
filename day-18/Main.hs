import Data.Foldable (traverse_)
import Data.List (find, foldl1')
import Data.Maybe (fromJust)
import Text.Parsec (between, char, digit, endBy1, endOfLine, eof, many1, (<|>))
import Text.Parsec.Text (Parser, parseFromFile)

data SnailfishNumber = Pair SnailfishNumber SnailfishNumber | Regular Int deriving (Eq)

instance Show SnailfishNumber where
  show (Regular n) = show n
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

data Flag = Unchanged | Split | Exploded (Maybe Int, Maybe Int) deriving (Show, Eq)

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ part1 input
  print $ part2 input

part1 :: [SnailfishNumber] -> Int
part1 input = magnitude $ foldl1' add input

part2 :: [SnailfishNumber] -> Int
part2 input = maximum $ [magnitude r | n1 <- input, n2 <- input, n1 /= n2, r <- [add n1 n2, add n2 n1]]

add :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
add a b = n where (n, _, _) = reduce $ Pair a b

reduce :: SnailfishNumber -> (SnailfishNumber, Flag, Int)
reduce n = fromJust $ find (\(_, a, _) -> a == Unchanged) (tail $ iterate (\(a, b, c) -> reduceStep (a, Unchanged, 0)) (n, Unchanged, 0))

reduce' n = takeWhile (\(_, a, _) -> a /= Unchanged) (tail $ iterate (\(a, b, c) -> reduceStep (a, Unchanged, 0)) (n, Unchanged, 0))

reduceStep :: (SnailfishNumber, Flag, Int) -> (SnailfishNumber, Flag, Int)
reduceStep (Regular n, flag, depth) = case flag of
  Unchanged | n >= 10 -> (split n, Split, depth)
  Unchanged -> (Regular n, flag, depth)
  Split -> (Regular n, flag, depth)
  Exploded (_, Just r) -> (Regular (n + r), Exploded (Nothing, Nothing), depth)
  Exploded (Just l, _) -> (Regular (n + l), Exploded (Nothing, Nothing), depth)
  Exploded _ -> (Regular n, flag, depth)
reduceStep (Pair a b, flag, depth) = case flag of
  Unchanged | depth >= 4 -> (Regular 0, Exploded (Just regularA, Just regularB), depth)
    where
      Regular regularA = a
      Regular regularB = b
  Unchanged -> case (flagLeft, flagRight) of
    (Exploded (leftExplosion, rightExplosion), _) -> (Pair a' b'', Exploded (leftExplosion, Nothing), depth)
      where
        (b'', _, _) = reduceStep (b, Exploded (Nothing, rightExplosion), depth + 1)
    (_, Exploded (leftExplosion, rightExplosion)) -> (Pair a'' b', Exploded (Nothing, rightExplosion), depth)
      where
        (a'', _, _) = reduceStep (a, Exploded (leftExplosion, Nothing), depth + 1)
    (_, _) -> (Pair a' b'', flag', depth)
      where
        (b'', flag', _) = reduceStep (b, flagLeft, depth + 1)
    where
      (a', flagLeft, _) = reduceStep (a, flag, depth + 1)
      (b', flagRight, _) = reduceStep (b, flag, depth + 1)
  Split -> (Pair a b, flag, depth)
  Exploded (Nothing, Nothing) -> (Pair a b, flag, depth)
  Exploded (l, r) -> (Pair a' b', Exploded (Nothing, Nothing), depth)
    where
      (a', _, _) = reduceStep (a, Exploded (Nothing, r), depth + 1)
      (b', _, _) = reduceStep (b, Exploded (l, Nothing), depth + 1)

split :: Int -> SnailfishNumber
split a = Pair (Regular $ floor half) (Regular $ ceiling half)
  where
    half = toRational a / 2

magnitude :: SnailfishNumber -> Int
magnitude (Pair a b) = (3 * magnitude a) + (2 * magnitude b)
magnitude (Regular a) = a

parser :: Parser [SnailfishNumber]
parser = snailfishNumberParser `endBy1` endOfLine <* eof

snailfishNumberParser :: Parser SnailfishNumber
snailfishNumberParser = between (char '[') (char ']') $ do
  first <- snailfishNumberParser <|> numberParser
  char ','
  second <- snailfishNumberParser <|> numberParser
  return $ Pair first second

numberParser :: Parser SnailfishNumber
numberParser = Regular . read <$> many1 digit