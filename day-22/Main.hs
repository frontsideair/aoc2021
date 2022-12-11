import Data.Functor (($>))
import Data.List (find, foldl')
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (char, digit, endBy1, endOfLine, eof, many1, space, string, try, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

type Range = (Int, Int)

type Range3 = (Range, Range, Range)

type Cuboid = (Range3, Bool)

intersectRange :: Range -> Range -> Maybe Range
intersectRange (start, end) (start', end') =
  if start' > end || start > end'
    then Nothing
    else Just (max start start', min end end')

intersectRange3 :: Range3 -> Range3 -> Maybe Range3
intersectRange3 (xRange, yRange, zRange) (xRange', yRange', zRange') = do
  xRange'' <- intersectRange xRange xRange'
  yRange'' <- intersectRange yRange yRange'
  zRange'' <- intersectRange zRange zRange'
  return (xRange'', yRange'', zRange'')

-- on + on = off, 100 + 100 = 100 + (100 - 10) = 190
-- on + off = off, 100 + 100 = 100 - 10 = 90
-- off + on = on
-- off + off = on
intersect :: Cuboid -> Cuboid -> Maybe Cuboid
intersect (range, on) (range', on') = do
  range'' <- intersectRange3 range range'
  return (range'', not on')

areaRange3 :: Range3 -> Int
areaRange3 ((xStart, xEnd), (yStart, yEnd), (zStart, zEnd)) =
  (xEnd - xStart + 1) * (yEnd - yStart + 1) * (zEnd - zStart + 1)

area :: Cuboid -> Int
area (range, on) = (if on then 1 else -1) * areaRange3 range

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  -- print input
  let input' = filter (\cuboid -> cuboid `within` (-50, 50)) input
  print $ sum $ area <$> foldl' f [] input'
  print $ sum $ area <$> foldl' f [] input
  return ()

within :: Cuboid -> Range -> Bool
within ((xRange, yRange, zRange), _) range = xRange `withinRange` range && yRange `withinRange` range && zRange `withinRange` range

withinRange :: Range -> Range -> Bool
withinRange (start, end) (start', end') = start' <= start && end' >= end

f :: [Cuboid] -> Cuboid -> [Cuboid]
f acc cuboid = acc ++ ([cuboid | snd cuboid]) ++ mapMaybe (intersect cuboid) acc

parser :: Parser [Cuboid]
parser = lineParser `endBy1` endOfLine <* eof

lineParser :: Parser Cuboid
lineParser = do
  on <- onOff
  space
  string "x="
  x <- rangeParser
  string ","
  string "y="
  y <- rangeParser
  string ","
  string "z="
  z <- rangeParser
  return ((x, y, z), on)

onOff :: Parser Bool
onOff = try (string "on" $> True) <|> try (string "off" $> False)

rangeParser :: Parser Range
rangeParser = do
  start <- int
  string ".."
  end <- int
  return (min start end, max start end)

int :: Parser Int
int = do
  sign <- (char '-' >> return negate) <|> return id
  num <- many1 digit
  return $ sign $ read num