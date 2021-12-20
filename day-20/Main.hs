{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Comonad.Representable.Store (ComonadStore (experiment), Store, StoreT (StoreT), store)
import Control.Comonad.Store (Comonad (extend), extract)
import Data.Distributive (Distributive (distribute))
import Data.Foldable (Foldable (foldl', toList))
import Data.Functor (($>))
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Rep (Representable (..), distributeRep)
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Text.Parsec (char, endBy1, endOfLine, eof, many1, (<|>))
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  (algorithm, image) <- parseFromFile parser "input.txt" >>= either (error . show) return
  let grid = mkGrid image
  let games = iterate (step (rule algorithm)) grid
  print $ part1 games
  print $ part2 games

part1 :: [Grid Pixel] -> Int
part1 games = litPixels $ games !! 2

part2 :: [Grid Pixel] -> Int
part2 games = litPixels $ games !! 50

newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 200

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i = v V.! (i `mod` gridSize)
  tabulate desc = VBounded $ V.generate gridSize desc

type Grid a = Store (Compose VBounded VBounded) a

type Pixel = Bool

mkGrid :: Matrix Pixel -> Grid Pixel
mkGrid xs = store lookup (0, 0)
  where
    lookup (x, y) = fromMaybe False (Matrix.safeGet (x + 1 - 50) (y + 1 - 50) xs)

type Rule = Grid Pixel -> Pixel

window :: (Int, Int) -> [(Int, Int)]
window (x, y) = [(x', y') | x' <- [x -1, x, x + 1], y' <- [y -1, y, y + 1]]

rule :: V.Vector Pixel -> Rule
rule algorithm g = algorithm V.! index
  where
    index = binaryToInt $ experiment window g

binaryToInt :: [Bool] -> Int
binaryToInt = foldl' (\y x -> 2 * y + (if x then 1 else 0)) 0

step :: Rule -> Grid Pixel -> Grid Pixel
step = extend

litPixels :: Grid Pixel -> Int
litPixels (StoreT (Identity (Compose g)) _) = length [cell | row <- toList g, cell <- toList row, cell]

parser :: Parser (V.Vector Pixel, Matrix Pixel)
parser = do
  algorithm <- V.fromList <$> many1 pixelParser
  endOfLine
  endOfLine
  image <- Matrix.fromLists <$> many1 pixelParser `endBy1` endOfLine
  eof
  return (algorithm, image)

pixelParser :: Parser Pixel
pixelParser = char '.' $> False <|> char '#' $> True