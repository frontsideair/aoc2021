{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (when)
import Data.Bifunctor (second)
import Data.Foldable (Foldable (foldl'))
import Data.Heap (Entry (Entry), Heap)
import qualified Data.Heap as Heap
import Data.List (elemIndex, foldl1', minimumBy, partition)
import Data.Map (Map)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as Map
import Data.Matrix (Matrix, (<->))
import qualified Data.Matrix as Matrix
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Parsec (eof, many1, newline, noneOf, parse, sepEndBy)
import Text.Parsec.String (Parser, parseFromFile)

toInsert :: [String]
toInsert =
  lines
    "  #D#C#B#A#\n\
    \  #D#B#A#C#"

part1 :: IO ()
part1 = do
  matrix <- parseFromFile parser "input.txt" >>= either (error . show) return
  let state = createState matrix
  let endgames = run state
  print $ cost $ minimumBy (comparing cost) endgames
  return ()

part2 :: IO ()
part2 = do
  (top, bottom) <- splitAt 3 . lines <$> readFile "input.txt"
  let input = unlines $ top ++ toInsert ++ bottom
  let matrix = either (error . show) id $ parse parser "string" input
  let state = createState matrix
  let endgames = run state
  print $ cost $ minimumBy (comparing cost) endgames
  return ()

main :: IO ()
main = do
  arg <- head . (++ ["all"]) <$> getArgs
  when (arg == "all" || arg == "part1") part1
  when (arg == "all" || arg == "part2") part2

run :: State -> [State]
run state =
  if isSuccessState state
    then return state
    else step state >>= run

energy :: Num p => Char -> p
energy 'A' = 1
energy 'B' = 10
energy 'C' = 100
energy 'D' = 1000
energy _ = error "no such amphipod"

type Coord = (Int, Int)

data State = State
  { matrix :: Matrix Char,
    amphipods :: Map Coord Char,
    corridors :: [Coord], -- size 7
    rooms :: Map Char [Coord], -- gate:rest
    cost :: Int,
    paths :: Map (Coord, Coord) (Set Coord)
  }

createState :: Matrix Char -> State
createState matrix =
  State
    { matrix = matrix,
      amphipods = amphipods,
      corridors = corridors,
      rooms = rooms,
      cost = 0,
      paths = paths
    }
  where
    room (x, y) = (,y) <$> take (Matrix.nrows matrix - 3) [x ..]
    rooms = Map.fromList $ second room <$> [('A', (3, 4)), ('B', (3, 6)), ('C', (3, 8)), ('D', (3, 10))]
    amphipods = Map.fromList $ (\(x, y) -> ((x, y), Matrix.getElem x y matrix)) <$> concat rooms
    corridors = [(2, 2), (2, 3), (2, 5), (2, 7), (2, 9), (2, 11), (2, 12)]
    empty = corridors ++ concat rooms
    paths = Map.fromList [((from, to), aStar from to matrix) | from <- empty, to <- empty, from /= to]

isAmphipodTerminal :: State -> Coord -> Bool
isAmphipodTerminal State {amphipods, rooms} coord = fromMaybe False $ do
  amphipod <- amphipods !? coord
  roomCoords <- rooms !? amphipod
  index <- elemIndex coord roomCoords
  let innerPart = drop index roomCoords
  return $ all (== Just amphipod) $ (amphipods !?) <$> innerPart

isSuccessState :: State -> Bool
isSuccessState state@State {rooms, amphipods} =
  all
    (\(amphipod, roomCoords) -> all (== Just amphipod) $ (amphipods !?) <$> roomCoords)
    (Map.toList rooms)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

reconstructPath :: Map Coord Coord -> Coord -> Set Coord
reconstructPath cameFrom current =
  case cameFrom !? current of
    Nothing -> Set.empty
    Just previous -> Set.insert current $ reconstructPath cameFrom previous

aStar :: Coord -> Coord -> Matrix Char -> Set Coord
aStar from to matrix = go openSet cameFrom gScore
  where
    openSet = Heap.singleton (Entry (manhattan from to) from)
    cameFrom = Map.empty
    gScore = Map.singleton from 0
    go openSet cameFrom gScore = case Heap.uncons openSet of
      Nothing -> Set.empty -- failure
      Just (Entry _ current, openSet') ->
        if current == to
          then reconstructPath cameFrom current
          else go openSet'' cameFrom' gScore'
        where
          (openSet'', cameFrom', gScore') = foldl' f (openSet', cameFrom, gScore) (neighborSpaces matrix current)
          f (o, c, g) n =
            let tentativeGScore = (case gScore !? current of Just n -> n; Nothing -> error "here") + 1
             in if case gScore !? n of Nothing -> True; Just x -> tentativeGScore < x
                  then
                    ( Heap.insert (Entry (tentativeGScore + manhattan n to) n) o,
                      Map.insert n current c,
                      Map.insert n tentativeGScore g
                    )
                  else (o, c, g)

neighborSpaces :: Matrix Char -> Coord -> Set Coord
neighborSpaces matrix (x, y) =
  Set.fromList $
    catMaybes $
      ( \(x, y) -> do
          char <- Matrix.safeGet x y matrix
          if isWalkable char then Just (x, y) else Nothing
      )
        <$> neighborCoords (x, y)

neighborCoords :: Coord -> [Coord]
neighborCoords (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isWalkable :: Char -> Bool
isWalkable char = char `notElem` [void, wall]
  where
    void = ' '
    wall = '#'

isPathUnblocked :: Set Coord -> State -> Bool
isPathUnblocked path State {amphipods} = path `Set.disjoint` Map.keysSet amphipods

isUnblocked :: State -> Coord -> Bool
isUnblocked State {amphipods} coord = coord `Map.notMember` amphipods

ownRoom :: State -> Coord -> [Coord]
ownRoom State {amphipods, rooms} coord = rooms ! (amphipods ! coord)

step :: State -> [State]
step state@State {amphipods, corridors, paths} = if null finalMoves then corridorMoves else finalMoves
  where
    (corridorAmphipods, roomAmphipods) = partition (`elem` corridors) (Map.keys amphipods)
    roomAmphipodsThatCanMove = filter (not . isAmphipodTerminal state) roomAmphipods
    corridorMoves =
      [ move state (Set.size path) from to
        | from <- roomAmphipodsThatCanMove,
          to <- corridors,
          let path = paths ! (from, to),
          isUnblocked state to,
          isPathUnblocked path state
      ]
    finalMoves =
      [ nextState
        | from <- corridorAmphipods ++ roomAmphipodsThatCanMove,
          to <- ownRoom state from,
          let path = paths ! (from, to),
          let nextState = move state (Set.size path) from to,
          isUnblocked state to,
          isPathUnblocked path state,
          isAmphipodTerminal nextState to
      ]

move :: State -> Int -> Coord -> Coord -> State
move state@State {amphipods, cost} steps from to =
  state
    { amphipods = Map.insert to amphipod (Map.delete from amphipods),
      cost = cost + (steps * energy amphipod)
    }
  where
    amphipod = amphipods ! from

{-
for each amphipod that is not at alone or with friend in its own room - amphipod coordinates, filter if terminal
find (reverse) paths to each corridor or room that belongs to the amphipod
filter paths that are blocked by another amphipod
generate these new states, calculating increased cost
repeat if list of states is not empty
-}

-- availableMoves
-- amphipod is not at its final state -> can move into its own room (if empty or has friend) or corridor
-- amphipod is at corridor -> can move into its own room (if empty or has friend)

parser :: Parser (Matrix Char)
parser = do
  rows <- row `sepEndBy` newline <* eof
  return $ foldl1' (<->) rows

row :: Parser (Matrix Char)
row = Matrix.fromList 1 13 . (++ repeat ' ') <$> many1 (noneOf "\n")
