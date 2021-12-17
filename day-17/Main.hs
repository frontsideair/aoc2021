import Text.Parsec (char, digit, endOfLine, eof, many1, optionMaybe, string)
import Text.Parsec.Text (Parser, parseFromFile)

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  print $ part1 input
  print $ part2 input

part1 :: ((Float, Float), (Float, Float)) -> Int
part1 ((xMin, xMax), (yMin, yMax)) = floor $ (yMin * (yMin + 1)) / 2

part2 :: ((Float, Float), (Float, Float)) -> Int
part2 ((xMin, xMax), (yMin, yMax)) = length [(xSpeed, ySpeed) | xSpeed <- [xSpeedMin .. xSpeedMax], ySpeed <- [ySpeedMin .. ySpeedMax], fallsWithinTarget xSpeed ySpeed]
  where
    xSpeedMin = closestPositiveRoot 1 1 (xMin * (-2))
    xSpeedMax = xMax
    ySpeedMin = yMin
    ySpeedMax = - yMin - 1
    fallsWithinTarget xSpeed ySpeed = any insideTarget $ takeWhile (not . overshot) $ iterate step ((xSpeed, ySpeed), (0, 0))
    step ((xSpeed, ySpeed), (x, y)) = ((max (xSpeed - 1) 0, ySpeed - 1), (x + xSpeed, y + ySpeed))
    overshot (_, (x, y)) = x > xMax || y < yMin
    insideTarget (_, (x, y)) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

closestPositiveRoot :: Float -> Float -> Float -> Float
closestPositiveRoot a b c = fromIntegral . ceiling . maximum $ roots a b c

roots :: Float -> Float -> Float -> [Float]
roots a b c = [(- b - delta) / (2 * a), (- b + delta) / (2 * a)]
  where
    delta = sqrt (b ^ 2 - 4 * a * c)

parser :: Parser ((Float, Float), (Float, Float))
parser = do
  string "target area: x="
  x <- rangeParser
  string ", y="
  y <- rangeParser
  endOfLine
  eof
  return (x, y)

rangeParser :: Parser (Float, Float)
rangeParser = do
  start <- integer
  string ".."
  end <- integer
  return (start, end)

integer :: Parser Float
integer = do
  sign <- optionMaybe (char '-')
  num <- many1 digit
  return $ case sign of
    Nothing -> read num
    Just _ -> negate $ read num