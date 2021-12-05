import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import Text.Parsec (digit, endBy, endOfLine, many1, string)
import Text.Parsec.Text (Parser, parseFromFile)

type Coordinate = (Int, Int)

type LineSegment = (Coordinate, Coordinate)

data Orientation = Horizontal | Vertical | Acute | Obtuse deriving (Eq, Show)

main :: IO ()
main = do
  parseResult <- parseFromFile parser "input.txt"
  let input = case parseResult of
        Left err -> error $ show err
        Right input -> input
  print $ part1 input
  print $ part2 input

part1 input =
  let map = foldl' (\m coordinate -> M.insertWith (const (+ 1)) coordinate 1 m) M.empty (input >>= lineSegmentPointsSimple)
   in M.size $ M.filter (>= 2) map

part2 input =
  let map = foldl' (\m coordinate -> M.insertWith (const (+ 1)) coordinate 1 m) M.empty (input >>= lineSegmentPoints)
   in M.size $ M.filter (>= 2) map

lineSegmentPointsSimple :: LineSegment -> [Coordinate]
lineSegmentPointsSimple lineSegment = case lineSegmentOrientation lineSegment of
  Horizontal -> horizontalPoints lineSegment
  Vertical -> verticalPoints lineSegment
  _ -> []

lineSegmentPoints :: LineSegment -> [Coordinate]
lineSegmentPoints lineSegment = case lineSegmentOrientation lineSegment of
  Horizontal -> horizontalPoints lineSegment
  Vertical -> verticalPoints lineSegment
  Acute -> acutePoints lineSegment
  Obtuse -> obtusePoints lineSegment

lineSegmentOrientation :: LineSegment -> Orientation
lineSegmentOrientation ((x1, y1), (x2, y2))
  | x1 == x2 = Horizontal
  | y1 == y2 = Vertical
  | compare x1 x2 == compare y1 y2 = Acute
  | otherwise = Obtuse

verticalPoints :: LineSegment -> [Coordinate]
verticalPoints ((x1, y1), (x2, y2)) = [(x, y1) | x <- [(min x1 x2) .. (max x1 x2)]]

horizontalPoints :: LineSegment -> [Coordinate]
horizontalPoints ((x1, y1), (x2, y2)) = [(x1, y) | y <- [(min y1 y2) .. (max y1 y2)]]

acutePoints :: LineSegment -> [Coordinate]
acutePoints ((x1, y1), (x2, y2)) = zip [(min x1 x2) .. (max x1 x2)] [(min y1 y2) .. (max y1 y2)]

obtusePoints :: LineSegment -> [Coordinate]
obtusePoints ((x1, y1), (x2, y2)) = zip [(min x1 x2) .. (max x1 x2)] (reverse [(min y1 y2) .. (max y1 y2)])

parser :: Parser [LineSegment]
parser = lineSegmentParser `endBy` endOfLine

lineSegmentParser :: Parser LineSegment
lineSegmentParser = do
  start <- coordinateParser
  _ <- string " -> "
  end <- coordinateParser
  return (start, end)

coordinateParser :: Parser Coordinate
coordinateParser = do
  x <- numberParser
  _ <- string ","
  y <- numberParser
  return (x, y)

numberParser :: Parser Int
numberParser = read <$> many1 digit