{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad ((>=>))
import Data.Foldable (Foldable (foldl'), traverse_)
import Data.Heap (Entry (Entry), Heap)
import qualified Data.Heap as Heap
import Data.List (minimumBy, sortOn)
import Data.Map (Map)
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec (endBy1, eof, noneOf, oneOf, skipMany)
import Text.Parsec.String (Parser, parseFromFile)

main :: IO ()
main = do
  [a, b, c, d, a', b', c', d'] <- parseFromFile parser "input.txt" >>= either (error . show) return
  let initialState = createState [a, b, c, d, a', b', c', d']
  print initialState
  -- print $ head $ sortOn cost $ filter isSuccessState $ [initialState] >>= (step >=> step >=> step >=> step >=> step >=> step >=> step >=> step >=> step)
  let endgames = run initialState
  print $ minimumBy (comparing cost) endgames
  return ()

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
  { amphipods :: Map Coord Char,
    spaces :: Set Coord,
    corridors :: [Coord], -- size 7
    rooms :: Map Char [Coord], -- [outer, inner]
    cost :: Int
  }

instance Show State where
  show State {cost, amphipods, rooms, corridors} =
    unlines
      [ "#############",
        "#" ++ s _1 ++ s _2 ++ "." ++ s _3 ++ "." ++ s _4 ++ "." ++ s _5 ++ "." ++ s _6 ++ s _7 ++ "#",
        "###" ++ s a1 ++ "#" ++ s b1 ++ "#" ++ s c1 ++ "#" ++ s d1 ++ "###",
        "  #" ++ s a2 ++ "#" ++ s b2 ++ "#" ++ s c2 ++ "#" ++ s d2 ++ "###",
        "  #########",
        "Cost: " ++ show cost
      ]
    where
      [a1, a2] = (amphipods !?) <$> (rooms ! 'A')
      [b1, b2] = (amphipods !?) <$> (rooms ! 'B')
      [c1, c2] = (amphipods !?) <$> (rooms ! 'C')
      [d1, d2] = (amphipods !?) <$> (rooms ! 'D')
      [_1, _2, _3, _4, _5, _6, _7] = (amphipods !?) <$> corridors
      s (Just a) = [a]
      s Nothing = "."

createState :: [Char] -> State
createState [a, b, c, d, a', b', c', d'] =
  State
    { amphipods = amphipods,
      spaces = Set.fromList $ roomSpaces ++ corridors ++ [(2, 4), (2, 6), (2, 8), (2, 10)],
      corridors = corridors,
      rooms = Map.fromList [('A', [(3, 4), (4, 4)]), ('B', [(3, 6), (4, 6)]), ('C', [(3, 8), (4, 8)]), ('D', [(3, 10), (4, 10)])],
      cost = 0
    }
  where
    amphipods = Map.fromList $ zip roomSpaces [a, b, c, d, a', b', c', d']
    roomSpaces = [(3, 4), (3, 6), (3, 8), (3, 10), (4, 4), (4, 6), (4, 8), (4, 10)]
    corridors = [(2, 2), (2, 3), (2, 5), (2, 7), (2, 9), (2, 11), (2, 12)]
createState _ = error "wrong number of amphipods"

isAmphipodTerminal :: State -> Coord -> Bool
isAmphipodTerminal State {amphipods, rooms} coord =
  let amphipod = amphipods ! coord
      [outer, inner] = rooms ! amphipod
      amphipodAtInnerPart = coord == inner
      amphipodAtOuterPartAndInnerPartHasFriend = coord == outer && (amphipods !? inner) == Just amphipod
   in amphipodAtInnerPart || amphipodAtOuterPartAndInnerPartHasFriend

isSuccessState :: State -> Bool
isSuccessState state@State {rooms, amphipods} =
  all
    (\(amphipod, [outer, inner]) -> (amphipods !? outer) == Just amphipod && (amphipods !? inner) == Just amphipod)
    (Map.toList rooms)

shortestPath :: State -> Coord -> Coord -> Map Coord Int
shortestPath state from to = go state knownDistances visited
  where
    knownDistances = Heap.singleton $ Entry 0 from
    visited = Map.empty
    go state knownDistances visited = case Heap.uncons knownDistances of
      Nothing -> visited
      Just (Entry distV v, knownDistances') -> go state knownDistances'' visited'
        where
          ns = filter (`Map.notMember` visited) (Set.toList $ neighborSpaces state v)
          dists = (+ distV) . const 1 <$> ns
          knownDistances'' = foldl' (updatePriority min) knownDistances' (zipWith Entry dists ns)
          visited' = Map.insert v distV visited

updatePriority :: (Ord p, Eq a) => (p -> p -> p) -> Heap (Entry p a) -> Entry p a -> Heap (Entry p a)
updatePriority f heap (Entry a b) = Heap.insert new tail
  where
    (head, tail) = Heap.partition (\(Entry _ b') -> b == b') heap
    Entry old _ = Heap.minimum head
    new = if Heap.null head then Entry a b else Entry (f old a) b

findPath :: Coord -> Coord -> State -> Set Coord
findPath from to state = findPath' (shortestPath state to from) Set.empty from to state

findPath' :: Map Coord Int -> Set Coord -> Coord -> Coord -> State -> Set Coord
findPath' shortestPathTree path from to state = if coordToMove == to then path' else findPath' shortestPathTree path' coordToMove to state
  where
    neighbors = Set.toList $ neighborSpaces state from `Set.difference` path
    coordToMove
      | null neighbors = error $ show (from, to, neighbors, path, shortestPathTree)
      | otherwise = minimumBy (comparing (shortestPathTree !?)) neighbors
    path' = Set.insert from path

neighborSpaces :: State -> Coord -> Set Coord
neighborSpaces State {spaces} (x, y) = Set.fromList [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] `Set.intersection` spaces

isPathUnblocked :: Set Coord -> State -> Bool
isPathUnblocked path State {amphipods} = path `Set.disjoint` Map.keysSet amphipods

isUnblocked :: State -> Coord -> Bool
isUnblocked State {amphipods} coord = coord `Map.notMember` amphipods

ownRoom :: State -> Coord -> [Coord]
ownRoom State {amphipods, rooms} coord = rooms ! (amphipods ! coord)

roomAmphipodsThatCanMove :: State -> [Coord]
roomAmphipodsThatCanMove state@State {amphipods, corridors} = filter (`notElem` corridors) $ filter (not . isAmphipodTerminal state) (Map.keys amphipods)

corridorAmphipods :: State -> [Coord]
corridorAmphipods State {amphipods, corridors} = filter (`elem` corridors) (Map.keys amphipods)

step :: State -> [State]
step state = if null finalMoves then corridorMoves else finalMoves
  where
    corridorMoves =
      [ move state (Set.size path) from to
        | from <- roomAmphipodsThatCanMove state,
          to <- filter (isUnblocked state) $ corridors state,
          let path = findPath to from state,
          isPathUnblocked path state
      ]
    finalMoves =
      [ nextState
        | from <- roomAmphipodsThatCanMove state,
          to <- filter (isUnblocked state) $ ownRoom state from,
          let path = findPath to from state,
          let nextState = move state (Set.size path) from to,
          isPathUnblocked path state && isAmphipodTerminal nextState to
      ]
        ++ [ nextState
             | from <- corridorAmphipods state,
               to <- filter (isUnblocked state) $ ownRoom state from,
               let path = findPath to from state,
               let nextState = move state (Set.size path) from to,
               isPathUnblocked path state && isAmphipodTerminal nextState to
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

parser :: Parser [Char]
parser = skip *> amphipod `endBy1` skip <* eof

skip :: Parser ()
skip = skipMany (noneOf "ABCD")

amphipod :: Parser Char
amphipod = oneOf "ABCD"