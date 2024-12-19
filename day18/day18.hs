import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace
import System.IO
import Prelude hiding (Left, Right)
import Data.Tuple

data Cell = Wall | Empty | Start | End deriving (Eq, Ord)

instance Show Cell where
  show :: Cell -> String
  show Wall = "#"
  show Empty = "."
  show Start = "S"
  show End = "E"

isWall :: Cell -> Bool
isWall Wall = True
isWall _ = False

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _ = False

isStart :: Cell -> Bool
isStart Start = True
isStart _ = False

isEnd :: Cell -> Bool
isEnd End = True
isEnd _ = False

type Grid = [[Cell]]

printGrid :: Grid -> IO ()
printGrid [] = putStrLn ""
printGrid (h : t) = do
  putStrLn (showLine h)
  printGrid t
  where
    showLine :: [Cell] -> String
    showLine = concatMap show

data Direction = Up | Down | Left | Right deriving (Eq, Ord, Enum)

instance Show Direction where
  show :: Direction -> String
  show Up = "^"
  show Down = "v"
  show Left = "<"
  show Right = ">"

isUp :: Direction -> Bool
isUp Up = True
isUp _ = False

isDown :: Direction -> Bool
isDown Down = True
isDown _ = False

isLeft :: Direction -> Bool
isLeft Left = True
isLeft _ = False

isRight :: Direction -> Bool
isRight Right = True
isRight _ = False

neighborsDir :: Direction -> [Direction]
neighborsDir Up = [Left, Right]
neighborsDir Down = [Left, Right]
neighborsDir Left = [Up, Down]
neighborsDir Right = [Up, Down]

type Position = (Int, Int)

parsePosition :: String -> Position
parsePosition s =
  let comma = fromJust $ elemIndex ',' s
   in (read $ drop (comma + 1) s, read $ take comma s)

loadFile :: String -> IO [Position]
loadFile file = do
  content <- readFile file
  return (map parsePosition (lines content))

outOfBounds :: Position -> Grid -> Bool
outOfBounds (line, column) grid =
  line < 0
    || line >= length grid
    || column < 0
    || column >= length (head grid)

getCellAt :: Position -> Grid -> Cell
getCellAt (line, column) grid = (grid !! line) !! column

isWallAt :: Position -> Grid -> Bool
isWallAt pos grid = isWall (getCellAt pos grid)

isEmptyAt :: Position -> Grid -> Bool
isEmptyAt pos grid = isEmpty (getCellAt pos grid)

isEndAt :: Position -> Grid -> Bool
isEndAt pos grid = isEnd (getCellAt pos grid)

nextPosition :: Position -> Direction -> Position
nextPosition (line, column) Up = (line - 1, column)
nextPosition (line, column) Down = (line + 1, column)
nextPosition (line, column) Left = (line, column - 1)
nextPosition (line, column) Right = (line, column + 1)

neighbors :: Vertex -> [Vertex]
neighbors pos =
  [nextPosition pos dir | dir <- [Up ..]]

findCell :: Cell -> Grid -> Position
findCell cell = go 0
  where
    go :: Int -> Grid -> Position
    go line [] = error "Searched cell is missing!"
    go line (h : t) =
      let column = elemIndex cell h
       in if isJust column then (line, fromJust column) else go (line + 1) t

cost :: Vertex -> Vertex -> Int
cost pos pos'
  | pos == pos' = error "Already seen this node"
  | pos /= pos' = 1

type Vertex = Position

type Unvisited = Set.Set Vertex

type Distances = Map.Map Vertex (Maybe Int)

dijkstra :: Grid -> Distances
dijkstra grid =
  dijkstraGo (initDistances grid 0 Map.empty) (initUnvisited grid 0 Set.empty)
  where
    initDistances :: Grid -> Int -> Distances -> Distances
    initDistances [] _ distances = distances
    initDistances (h : t) line distances =
      initDistances t (line + 1) (initLine h line 0 distances)
      where
        addTo :: Position -> Maybe Int -> Distances -> Distances
        addTo pos value = Map.insert pos (value)

        initLine :: [Cell] -> Int -> Int -> Distances -> Distances
        initLine [] _ _ distances = distances
        initLine (Wall : t) line column distances =
          -- Walls can never be visited; we do not store distances for them
          initLine t line (column + 1) distances
        initLine (Start : t) line column distances =
          initLine t line (column + 1) $
            Map.insert (line, column) (Just 0) $
              addTo (line, column) Nothing distances
        initLine (_ : t) line column distances =
          initLine t line (column + 1) $ addTo (line, column) Nothing distances

    initUnvisited :: Grid -> Int -> Unvisited -> Unvisited
    initUnvisited [] _ unvisited = unvisited
    initUnvisited (h : t) line unvisited =
      initUnvisited t (line + 1) (initLine h line 0 unvisited)
      where
        addTo :: Position -> Unvisited -> Unvisited
        addTo = Set.insert

        initLine :: [Cell] -> Int -> Int -> Unvisited -> Unvisited
        initLine [] _ _ unvisited = unvisited
        initLine (Wall : t) line column unvisited =
          -- Walls can never be visited
          initLine t line (column + 1) unvisited
        initLine (_ : t) line column unvisited =
          initLine t line (column + 1) $ addTo (line, column) unvisited

    findMinDistance :: Unvisited -> Distances -> Vertex
    findMinDistance unvisited distances =
      foldl
        ( \old new ->
            if fromMaybe maxBound (distances Map.! new)
              < fromMaybe maxBound (distances Map.! old)
              then new
              else old
        )
        (Set.findMin unvisited)
        unvisited

    markVisited :: Vertex -> Unvisited -> Unvisited
    markVisited = Set.delete

    unvisitedNeighbors :: Vertex -> Unvisited -> [Vertex]
    unvisitedNeighbors pos unvisited =
      filter (`Set.member` unvisited) (neighbors pos)

    updateNeighbors :: Vertex -> [Vertex] -> Distances -> Distances
    updateNeighbors _ [] distances = distances
    updateNeighbors vertex (neighbor : t) distances =
      let costToVertex = fromJust $ distances Map.! vertex
          costToNeighbor = costToVertex + cost vertex neighbor
          currentCost = distances Map.! neighbor
          nextDistances
            | isNothing currentCost || costToNeighbor < fromJust currentCost =
                Map.insert
                  neighbor
                  (Just costToNeighbor)
                  distances
            | otherwise = distances
       in updateNeighbors vertex t nextDistances

    dijkstraGo :: Distances -> Unvisited -> Distances
    dijkstraGo distances unvisited
      | null unvisited = distances
      | otherwise =
          let nextVertex = findMinDistance unvisited distances
              neighbors = unvisitedNeighbors nextVertex unvisited
              newDistances = updateNeighbors nextVertex neighbors distances
              newUnvisited = markVisited nextVertex unvisited
           in if isEndAt nextVertex grid || isNothing (distances Map.! nextVertex)
                then distances
                else dijkstraGo newDistances newUnvisited

findStart :: Grid -> Position
findStart = findCell Start

findEnd :: Grid -> Position
findEnd = findCell End

costShortestPath :: Grid -> Distances -> Int
costShortestPath grid distances =
  let end = findEnd grid
   in fromMaybe maxBound $ distances Map.! end

changeCell :: Grid -> Position -> Cell -> Grid
changeCell grid (line, column) cell =
  let linesBefore = take line grid
      lineToChange = grid !! line
      linesAfter = drop (line + 1) grid
      columnsBefore = take column lineToChange
      columnsAfter = drop (column + 1) lineToChange
      newLine = columnsBefore ++ [cell] ++ columnsAfter
    in linesBefore ++ [newLine] ++ linesAfter

buildGrid :: Int -> [Position] -> Int -> Int -> Grid
buildGrid n corruptions nLines nColumns =
  changeCell
    (changeCell (go n corruptions) (0, 0) Start)
    (nLines - 1, nColumns - 1)
    End
  where
    go :: Int -> [Position] -> Grid
    go 0 _ = replicate nLines (replicate nColumns Empty)
    go n (corruption : t) = changeCell (go (n - 1) t) corruption Wall

puzzle1 :: Grid -> Distances -> Int
puzzle1 = costShortestPath

canBeReached :: Grid -> Distances -> Bool
canBeReached grid distances =
  let end = findEnd grid
   in isJust (distances Map.! end)

-- Possible optimizations:
-- Only restart Dijkstra if the new wall is on the current shortest path
-- Use a priority queue in Dijkstra
continueUntilBlocked :: Grid -> [Position] -> Position
continueUntilBlocked grid [] = error "End cell can always be reached"
continueUntilBlocked grid (corruption : t) =
  let grid' = changeCell grid (traceShowId corruption) Wall
      distances = dijkstra grid'
   in if canBeReached grid' distances
        then continueUntilBlocked grid' t
        else corruption

puzzle2 :: Grid -> [Position] -> Position
puzzle2 grid corruptions = swap $ continueUntilBlocked grid corruptions

test :: IO ()
test = do
  corruptions <- loadFile "test"
  print corruptions
  let grid = buildGrid 12 corruptions 7 7
  printGrid grid
  let distances = dijkstra grid
  print (costShortestPath grid distances)
  let grid' = changeCell (changeCell (changeCell (changeCell (changeCell (changeCell (changeCell (changeCell (changeCell grid (2,1) Wall) (5,5) Wall) (5,2) Wall) (5,6) Wall) (4,1) Wall) (4, 0) Wall) (4,6) Wall) (1,1) Wall) (1,6) Wall
  printGrid grid'
  print (canBeReached grid' (dijkstra grid'))

main :: IO ()
main =
  do
    print "Test"
    corruptions <- loadFile "test"
    let grid = buildGrid 12 corruptions 7 7
    let distances = dijkstra grid
    print (puzzle1 grid distances)
    print (puzzle2 grid (drop 12 corruptions))

    print "Input"
    corruptions <- loadFile "input"
    let grid = buildGrid 1024 corruptions 71 71
    let distances = dijkstra grid
    print (puzzle1 grid distances)
    print (puzzle2 grid (drop 1024 corruptions))
