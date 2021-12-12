import Data.Char (isLower, isUpper)
import Data.Graph.Connectivity (areConnected)
import Data.Graph.Types (Edge (Edge), Graph (adjacentVertices))
import Data.Graph.UGraph (UGraph, fromEdgesList, prettyPrint)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (char, endBy, endOfLine, eof, letter, many1, string, try, (<|>))
import Text.Parsec.Text (Parser, parseFromFile)

main :: IO ()
main = do
  input <- parseFromFile parser "input.txt" >>= either (error . show) return
  let graph = fromEdgesList $ (\(l, r) -> Edge l r ()) <$> input
  print $ part1 graph
  print $ part2 graph

part1 :: UGraph String () -> Int
part1 graph = length $ loop (extendPath graph) [["start"]]

part2 :: UGraph String () -> Int
part2 graph = length $ loop (extendPath' graph) [(["start"], False)]

loop :: Eq a => (a -> [a]) -> [a] -> [a]
loop f paths = if next == paths then paths else loop f next
  where
    next = paths >>= f

extendPath :: UGraph String () -> [String] -> [[String]]
extendPath graph ("end" : path) = ["end" : path]
extendPath graph path = (: path) <$> filter (canVisit path) (adjacentVertices graph (head path))

canVisit :: [String] -> String -> Bool
canVisit path cave = bigCave cave || (cave `notElem` path)

extendPath' :: UGraph String () -> ([String], Bool) -> [([String], Bool)]
extendPath' graph ("end" : path, twice) = [("end" : path, twice)]
extendPath' graph (path, True) = (,True) <$> extendPath graph path
extendPath' graph (path, False) = ((,False) <$> extendPath graph path) ++ ((\v -> (v : path, True)) <$> filter onceVisit (adjacentVertices graph (head path)))
  where
    onceVisit cave = not (canVisit path cave) && cave /= "start"

bigCave :: String -> Bool
bigCave = all isUpper

parser :: Parser [(String, String)]
parser = lineParser `endBy` endOfLine <* eof

lineParser :: Parser (String, String)
lineParser = do
  left <- cave
  char '-'
  right <- cave
  return (left, right)

cave :: Parser String
cave = try (string "start") <|> try (string "end") <|> many1 letter