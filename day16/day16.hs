import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace
import System.IO
import Prelude hiding (Left, Right)

data Cell = Wall | Empty | Start | End deriving (Eq, Ord)

instance Show Cell where
  show :: Cell -> String
  show Wall = "#"
  show Empty = "."
  show Start = "S"
  show End = "E"

charToCell :: Char -> Cell
charToCell '#' = Wall
charToCell '.' = Empty
charToCell 'S' = Start
charToCell 'E' = End

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

parseGridLine :: String -> [Cell]
parseGridLine = map charToCell

printGrid :: Grid -> IO ()
printGrid [] = putStrLn ""
printGrid (h : t) = do
  putStrLn (showLine h)
  printGrid t
  where
    showLine :: [Cell] -> String
    showLine = concatMap show

data Direction = Up | Down | Left | Right deriving (Eq, Ord)

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

loadFile :: String -> IO Grid
loadFile file = do
  content <- readFile file
  return (map parseGridLine (lines content))

type Position = (Int, Int)

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

neighborsWithDir :: Vertex -> [Vertex]
neighborsWithDir (pos, dir) =
  (nextPosition pos dir, dir) : map (pos,) (neighborsDir dir)

findCell :: Cell -> Grid -> Position
findCell cell = go 0
  where
    go :: Int -> Grid -> Position
    go line [] = error "Robot is missing!"
    go line (h : t) =
      let column = elemIndex cell h
       in if isJust column then (line, fromJust column) else go (line + 1) t

cost :: Vertex -> Vertex -> Int
cost (pos, dir) (pos', dir')
  | pos == pos' && dir == dir' = error "Already seen this node"
  | pos == pos' && dir /= dir' = 1000
  | pos /= pos' && dir == dir' = 1
  | otherwise = error "Skipping nodes"

type Vertex = (Position, Direction)

type Unvisited = Set.Set Vertex

type Distances = Map.Map Vertex (Maybe Int, [Vertex])

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
        addTo pos value distances =
          let distancesUp =
                Map.insert (pos, Up) (value, []) distances
              distancesDown =
                Map.insert (pos, Down) (value, []) distancesUp
              distancesLeft =
                Map.insert (pos, Left) (value, []) distancesDown
              distancesRight =
                Map.insert (pos, Right) (value, []) distancesLeft
           in distancesRight

        initLine :: [Cell] -> Int -> Int -> Distances -> Distances
        initLine [] _ _ distances = distances
        initLine (Wall : t) line column distances =
          -- Walls can never be visited; we do not store distances for them
          initLine t line (column + 1) distances
        initLine (Start : t) line column distances =
          initLine t line (column + 1) $
            Map.insert ((line, column), Right) (Just 0, []) $
              addTo (line, column) Nothing distances
        initLine (_ : t) line column distances =
          initLine t line (column + 1) $ addTo (line, column) Nothing distances

    initUnvisited :: Grid -> Int -> Unvisited -> Unvisited
    initUnvisited [] _ unvisited = unvisited
    initUnvisited (h : t) line unvisited =
      initUnvisited t (line + 1) (initLine h line 0 unvisited)
      where
        addTo :: Position -> Unvisited -> Unvisited
        addTo pos unvisited =
          let unvisitedUp =
                Set.insert (pos, Up) unvisited
              unvisitedDown =
                Set.insert (pos, Down) unvisitedUp
              unvisitedLeft =
                Set.insert (pos, Left) unvisitedDown
              unvisitedRight =
                Set.insert (pos, Right) unvisitedLeft
           in unvisitedRight

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
            if fromMaybe maxBound (fst $ distances Map.! new)
              < fromMaybe maxBound (fst $ distances Map.! old)
              then new
              else old
        )
        (Set.findMin unvisited)
        unvisited

    markVisited :: Vertex -> Unvisited -> Unvisited
    markVisited = Set.delete

    unvisitedNeighbors :: Vertex -> Unvisited -> [Vertex]
    unvisitedNeighbors pos unvisited =
      filter (`Set.member` unvisited) (neighborsWithDir pos)

    updateNeighbors :: Vertex -> [Vertex] -> Distances -> Distances
    updateNeighbors _ [] distances = distances
    updateNeighbors vertex (neighbor : t) distances =
      let costToVertex = fromJust $ fst $ distances Map.! vertex
          costToNeighbor = costToVertex + cost vertex neighbor
          currentCost = fst $ distances Map.! neighbor
          nextDistances
            | isNothing currentCost || costToNeighbor < fromJust currentCost =
                Map.insert
                  neighbor
                  (Just costToNeighbor, [vertex])
                  distances
            | costToNeighbor == fromJust currentCost =
                Map.adjust
                  (\(cost, pred) -> (cost, (vertex : pred)))
                  neighbor
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
           in dijkstraGo newDistances newUnvisited

findStart :: Grid -> Position
findStart = findCell Start

findEnd :: Grid -> Position
findEnd = findCell End

puzzle1 :: Grid -> Distances -> Int
puzzle1 grid distances =
  let end = findEnd grid
   in minimum
        [ fromMaybe maxBound $ fst $ distances Map.! (end, dir) | dir <- [Up, Down, Left, Right]
        ]

positionsOnPaths :: Distances -> Vertex -> [Position]
positionsOnPaths distances vertex
  | null $ snd $ distances Map.! vertex = [fst vertex]
  | otherwise =
      fst vertex
        : concatMap (positionsOnPaths distances) (snd $ distances Map.! vertex)

puzzle2 :: Grid -> Distances -> Int
puzzle2 grid distances =
  let end = findEnd grid
      minimumCost =
        minimum [fromJust $ fst $ distances Map.! (end, dir) | dir <- [Up, Down, Left, Right]]
      positionsOrNull :: Direction -> [Position]
      positionsOrNull dir
        | minimumCost == fromJust (fst (distances Map.! (end, dir))) =
            positionsOnPaths distances (end, Up)
        | otherwise = []
      allPositions =
        positionsOrNull Up
          ++ positionsOrNull Down
          ++ positionsOrNull Left
          ++ positionsOrNull Right
   in length $ nub allPositions

test :: IO ()
test = do
  grid <- loadFile "test"
  printGrid grid
  print (findStart grid)
  print (findEnd grid)
  print (positionsOnPaths (dijkstra grid) ((13, 13), Up))

main :: IO ()
main =
  do
    print "Test"
    grid <- loadFile "test"
    let distances = dijkstra grid
    print (puzzle1 grid distances)
    print (puzzle2 grid distances)

    print "Input"
    grid <- loadFile "input"
    let distances = dijkstra grid
    print (puzzle1 grid distances)
    print (puzzle2 grid distances)
