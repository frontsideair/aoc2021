{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Comonad (Comonad (extend, extract))
import Control.Comonad.Representable.Store (ComonadStore (experiment), Store, StoreT (StoreT), store)
import Data.Distributive (Distributive (distribute))
import Data.Foldable (Foldable (toList))
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.List (findIndex)
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Text.Parsec (count, digit, endBy1, endOfLine, eof, many1)
import Text.Parsec.Text (Parser, parseFromFile)

newtype VBounded a = VBounded (V.Vector a) deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 10

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc

type Grid a = Store (Compose VBounded VBounded) a

type Octopus = (Int, Bool)

mkGrid :: Matrix Int -> Grid Octopus
mkGrid xs = store lookup (0, 0) where lookup (x, y) = (xs ! (x + 1, y + 1), False)

type Rule = Grid Octopus -> Octopus

neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (x, y) = [(x', y') | x' <- [x -1, x, x + 1], x' >= 0, x' < gridSize, y' <- [y -1, y, y + 1], y' >= 0, y' < gridSize, (x', y') /= (x, y)]

turnRule :: Rule
turnRule g = (energy + 1, flashed) where (energy, flashed) = extract g

cascadeRule :: Rule
cascadeRule g = (energy + numFlashingNeighbors, energy > 9)
  where
    (energy, flashed) = extract g
    neighbours = experiment neighbourCoords g
    numFlashingNeighbors = length (filter isFlashing neighbours)

isFlashing :: Octopus -> Bool
isFlashing (energy, flashed) = not flashed && (energy > 9)

finalRule :: Rule
finalRule g = (if energy > 9 then 0 else energy, False) where (energy, _) = extract g

step :: Rule -> Grid Octopus -> Grid Octopus
step = extend

gameStep :: Grid Octopus -> Grid Octopus
gameStep = step finalRule . loop (step cascadeRule) . step turnRule

loop :: (Grid Octopus -> Grid Octopus) -> Grid Octopus -> Grid Octopus
loop stepper game =
  if countCells isFlashing game == 0
    then game
    else loop stepper (stepper game)

countCells :: (Octopus -> Bool) -> Grid Octopus -> Int
countCells f (StoreT (Identity (Compose g)) _) = length [cell | row <- toList g, cell <- toList row, f cell]

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let grid = mkGrid input
  let games = iterate gameStep grid
  print $ part1 games
  print $ part2 games

part1 :: [Grid Octopus] -> Int
part1 games = sum $ countCells ((== 0) . fst) <$> take 101 games

part2 :: [Grid Octopus] -> Int
part2 = fromJust . findIndex (\g -> countCells ((== 0) . fst) g == 100)

parser :: Parser (Matrix Int)
parser = Matrix.fromLists <$> line `endBy1` endOfLine <* eof

line :: Parser [Int]
line = many1 octopus

octopus :: Parser Int
octopus = read <$> count 1 digit