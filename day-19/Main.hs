import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Float (int2Double)
import Text.Parsec (char, digit, endBy1, endOfLine, eof, many1, optionMaybe, sepBy1, string)
import Text.Parsec.String (Parser, parseFromFile)

type Coord = (Int, Int, Int)

type Scanner = Set Coord

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let (beacons, scanners) = go (Set.empty, []) [(head input, (0, 0, 0))] (tail input)
  print $ length beacons
  let maxDist = maximum $ [manhattan s1 s2 | s1 <- scanners, s2 <- scanners]
  print maxDist

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

go :: (Scanner, [Coord]) -> [(Scanner, Coord)] -> [Scanner] -> (Scanner, [Coord])
go acc [] pending = acc
go (acc, coords) ((scanner, coord) : rest) pending = go acc' queue' pending'
  where
    (overlapping, pending') = List.partition (\candidate -> length (f (distances scanner) (distances candidate)) >= 12) pending
    queue' = rest ++ (adjust <$> overlapping)
    acc' = (Set.union acc scanner, coord : coords)
    f' a b = Set.toList $ f a b
    adjust candidate = (Set.fromList $ translatePoint dist . rotate <$> Set.toList candidate, (translatePoint dist . rotate) (0, 0, 0))
      where
        leftDistances = distances scanner
        rightDistances = distances candidate
        overlappingPointsLeft = f' leftDistances rightDistances
        overlappingPointsRight = f' rightDistances leftDistances
        (rotate, dist) = findRotationAndDist overlappingPointsLeft overlappingPointsRight

findRotationAndDist :: [Coord] -> [Coord] -> (Coord -> Coord, Coord)
findRotationAndDist left right =
  case [ (rot, dist)
         | rot <- pointRotations,
           l <- left,
           r <- right,
           let dist = l `subtractPoint` rot r,
           let intersect = Set.fromList left `Set.intersection` Set.fromList (translatePoint dist . rot <$> right),
           Set.size intersect >= 12
       ] of
    [] -> error $ "no rotation found" ++ show (left, right)
    (x : _) -> x

subtractPoint :: Coord -> Coord -> Coord
subtractPoint (x, y, z) (x', y', z') = (x - x', y - y', z - z')

translatePoint :: Coord -> Coord -> Coord
translatePoint (x, y, z) (x', y', z') = (x + x', y + y', z + z')

f :: (Ord a, Ord k) => Map k (a, a) -> Map k b -> Set a
f a b = points
  where
    xs = Map.intersection a b
    (left, right) = unzip $ Map.elems xs
    points = Set.fromList left `Set.union` Set.fromList right

distance :: Coord -> Coord -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ f x1 x2 + f y1 y2 + f z1 z2
  where
    f a1 a2 = (int2Double a1 - int2Double a2) ^ 2

distances :: Scanner -> Map Double (Coord, Coord)
distances scanner = Map.fromList [(distance p1 p2, (p1, p2)) | p1 <- Set.toList scanner, p2 <- Set.toList scanner, p1 < p2]

pointRotations :: [Coord -> Coord]
pointRotations =
  [ id,
    x,
    y,
    x . x,
    x . y,
    y . x,
    y . y,
    x . x . x,
    x . x . y,
    x . y . x,
    x . y . y,
    y . x . x,
    y . y . x,
    y . y . y,
    x . x . x . y,
    x . x . y . x,
    x . x . y . y,
    x . y . x . x,
    x . y . y . y,
    y . x . x . x,
    y . y . y . x,
    x . x . x . y . x,
    x . y . x . x . x,
    x . y . y . y . x
  ]
  where
    x (x, y, z) = (x, - z, y)
    y (x, y, z) = (z, y, - x)

parser :: Parser [Scanner]
parser = do
  scanners <- scannerParser `sepBy1` endOfLine
  eof
  return scanners

scannerParser :: Parser Scanner
scannerParser = do
  string "--- scanner "
  many1 digit
  string " ---"
  endOfLine
  scanners <- beaconParser `endBy1` endOfLine
  return $ Set.fromList scanners

beaconParser :: Parser Coord
beaconParser = do
  x <- double
  char ','
  y <- double
  char ','
  z <- double
  return (x, y, z)

double :: Parser Int
double = do
  sign <- optionMaybe (char '-')
  num <- read <$> many1 digit
  return $ case sign of
    Nothing -> num
    Just _ -> - num