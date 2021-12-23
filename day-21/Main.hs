import Data.List (find, foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Text.Parsec (digit, endOfLine, eof, many1, string)
import Text.Parsec.String (Parser, parseFromFile)

type Player = (Int, Int) -- (position, score)

type GameState = (Player, Player, Bool) -- p1, p2, p1's turn

main :: IO ()
main = do
  (p1, p2) <- parseFromFile parser "input.txt" >>= either (error . show) return
  let initial = ((p1, 0), (p2, 0), True)
  print $ part1 initial
  print $ part2 initial

part1 :: (Player, Player, Bool) -> Int
part1 initial = fromJust $ result <$> find (goalReached 1000 . fst) r
  where
    r = iterate step (initial, groupN 3 [1 ..])

part2 :: (Player, Player, Bool) -> Int
part2 initial = max p1 p2
  where
    (p1, p2) = step' 21 initial

goalReached :: Int -> GameState -> Bool
goalReached goal (p1, p2, _) = isWinning goal p1 || isWinning goal p2

isWinning :: Int -> Player -> Bool
isWinning goal (_, score) = score >= goal

result :: (GameState, [[Int]]) -> Int
result (((_, s1), (_, s2), _), rolls) = loserScore * timesRolled
  where
    loserScore = if s1 >= 1000 then s2 else s1
    timesRolled = head (head rolls) - 1

step :: (GameState, [[Int]]) -> (GameState, [[Int]])
step (_, []) = error "Empty input"
step (game, rolls : rest) = (move' game (sum rolls), rest)

dirac :: [(Int, Int)]
dirac = Map.toList $ Map.fromListWith (+) [(a + b + c, 1) | a <- [1, 2, 3], b <- [1, 2, 3], c <- [1, 2, 3]]

step' :: Int -> GameState -> (Int, Int)
step' goal game = if goalReached goal game then score else foldl' (both (+)) (0, 0) [both (*) (occurences, occurences) (step' goal (move' game increment)) | (increment, occurences) <- dirac]
  where
    (p1, _, _) = game
    score = if isWinning goal p1 then (1, 0) else (0, 1)

both :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
both f (x, y) (a, b) = (f x a, f y b)

move' :: GameState -> Int -> GameState
move' (p1, p2, True) increment = (move p1 increment, p2, False)
move' (p1, p2, False) increment = (p1, move p2 increment, True)

move :: Player -> Int -> Player
move (position, score) increment = (position', score')
  where
    position' = (position + increment) `rem'` 10
    score' = score + position'

rem' :: Integral a => a -> a -> a
a `rem'` b = ((a -1) `rem` b) + 1

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = l : groupN n r
  where
    (l, r) = splitAt n xs

parser :: Parser (Int, Int)
parser = (,) <$> player <*> player <* eof

player :: Parser Int
player = do
  string "Player "
  digit
  string " starting position: "
  p <- number
  endOfLine
  return p

number :: Parser Int
number = read <$> many1 digit