import Control.Monad
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Debug.Trace
import System.IO
import Prelude hiding (Left, Right)

data Cell = Wall | Crate Int | Empty | Robot deriving (Eq, Ord)

instance Show Cell where
  show :: Cell -> String
  show Wall = "#"
  show (Crate 0) = "O"
  show (Crate 1) = "["
  show (Crate 2) = "]"
  show Empty = "."
  show Robot = "@"

charToCell :: Char -> Cell
charToCell '#' = Wall
charToCell 'O' = Crate 0
charToCell '.' = Empty
charToCell '@' = Robot

isWall :: Cell -> Bool
isWall Wall = True
isWall _ = False

isCrate :: Cell -> Bool
isCrate (Crate _) = True
isCrate _ = False

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _ = False

crateNumber :: Cell -> Int
crateNumber (Crate a) = a

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

charToDirection :: Char -> Direction
charToDirection '^' = Up
charToDirection 'v' = Down
charToDirection '<' = Left
charToDirection '>' = Right

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

type Movements = [Direction]

parseDirections :: String -> Movements
parseDirections = map charToDirection

loadFile :: String -> IO (Grid, Movements)
loadFile file = do
  content <- readFile file
  let l = lines content
  let endGrid = fromJust $ elemIndex "" l
  let grid = map parseGridLine (take endGrid l)
  let movements = concatMap parseDirections (drop endGrid l)
  return (grid, movements)

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

isCrateAt :: Position -> Grid -> Bool
isCrateAt pos grid = isCrate (getCellAt pos grid)

crateNumberAt :: Position -> Grid -> Int
crateNumberAt pos grid = crateNumber (getCellAt pos grid)

nextPosition :: Position -> Direction -> Position
nextPosition (line, column) Up = (line - 1, column)
nextPosition (line, column) Down = (line + 1, column)
nextPosition (line, column) Left = (line, column - 1)
nextPosition (line, column) Right = (line, column + 1)

findRobot :: Grid -> Position
findRobot = go 0
  where
    go :: Int -> Grid -> Position
    go line [] = error "Robot is missing!"
    go line (h : t) =
      let column = elemIndex Robot h
       in if isJust column then (line, fromJust column) else go (line + 1) t

moveOneCell :: Grid -> Cell -> Position -> Direction -> Grid
moveOneCell grid cell (line, column) Up =
  let currentCol = grid !! line
      targetCol = grid !! (line - 1)
   in take (line - 1) grid
        ++ [take column targetCol ++ [cell] ++ drop (column + 1) targetCol]
        ++ [take column currentCol ++ [Empty] ++ drop (column + 1) currentCol]
        ++ drop (line + 1) grid
moveOneCell grid cell (line, column) Down =
  let currentCol = grid !! line
      targetCol = grid !! (line + 1)
   in take line grid
        ++ [take column currentCol ++ [Empty] ++ drop (column + 1) currentCol]
        ++ [take column targetCol ++ [cell] ++ drop (column + 1) targetCol]
        ++ drop (line + 2) grid
moveOneCell grid cell (line, column) Left =
  transpose $ moveOneCell (transpose grid) cell (column, line) Up
moveOneCell grid cell (line, column) Right =
  transpose $ moveOneCell (transpose grid) cell (column, line) Down

type MoveCrates = Grid -> Position -> Direction -> (Grid, Bool)

moveCrates :: MoveCrates
moveCrates grid pos dir
  | isWallAt pos grid = (grid, False)
  | isEmptyAt pos grid = (grid, True)
  | otherwise =
      let (grid', success) = moveCrates grid (nextPosition pos dir) dir
       in if not success
            then (grid', False)
            else (moveOneCell grid' (Crate 0) pos dir, True)

step :: MoveCrates -> Grid -> Position -> Direction -> (Grid, Position)
step moveCrates grid pos dir
  | outOfBounds (nextPosition pos dir) grid = (grid, pos)
  | isWallAt (nextPosition pos dir) grid = (grid, pos)
  | isEmptyAt (nextPosition pos dir) grid =
      (moveOneCell grid Robot pos dir, nextPosition pos dir)
  | otherwise =
      let (grid', success) = moveCrates grid (nextPosition pos dir) dir
       in if not success
            then (grid, pos)
            else (moveOneCell grid' Robot pos dir, nextPosition pos dir)

applyMoves :: MoveCrates -> Grid -> Movements -> Position -> Grid
applyMoves _ grid [] _ = grid
applyMoves moveCrates grid (h : t) pos =
  let (newGrid, newRobot) = step moveCrates grid pos h
   in applyMoves moveCrates newGrid t newRobot

coordinate :: Grid -> Position -> Int
coordinate grid (line, column)
  | isCrateAt (line, column) grid = 100 * line + column
  | otherwise = 0

score :: Grid -> Int
score grid =
  sum
    [ coordinate grid (line, column)
      | line <- [1 .. length grid - 2], -- The border of the grid is only walls -> we can skip the walls
        column <- [1 .. length (head grid) - 2]
    ]

puzzle1 :: Grid -> Movements -> Int
puzzle1 grid moves = score $ applyMoves moveCrates grid moves (findRobot grid)

doubleGrid :: Grid -> Grid
doubleGrid [] = []
doubleGrid (h : t) = go h : doubleGrid t
  where
    go :: [Cell] -> [Cell]
    go [] = []
    go (Wall : t) = [Wall, Wall] ++ go t
    go (Crate 0 : t) = [Crate 1, Crate 2] ++ go t
    go (Empty : t) = [Empty, Empty] ++ go t
    go (Robot : t) = [Robot, Empty] ++ go t

moveBigCrates :: Grid -> Position -> Direction -> (Grid, Bool)
moveBigCrates grid pos dir
  | isWallAt pos grid = (grid, False)
  | isEmptyAt pos grid = (grid, True)
  | isRight dir || isLeft dir -- Pushing to the right or to the left is as for puzzle 1
    =
      let (grid', success) = moveBigCrates grid (nextPosition pos dir) dir
       in if not success
            then (grid', False)
            else (moveOneCell grid' (Crate (crateNumberAt pos grid)) pos dir, True)
  | isUp dir || isDown dir -- Pushing upwards or downwards requires to try to push both parts of the crate independently
    =
      let crate1Pos =
            if crateNumberAt pos grid == 1 then pos else nextPosition pos Left
          crate2Pos =
            if crateNumberAt pos grid == 1 then nextPosition pos Right else pos
          (gridAfter1, successAfter1) =
            moveBigCrates grid (nextPosition crate1Pos dir) dir
          (gridAfter2, successAfter2) =
            if not successAfter1
              then (gridAfter1, False)
              else moveBigCrates gridAfter1 (nextPosition crate2Pos dir) dir
       in if not successAfter2
            then (grid, False)
            else
              ( moveOneCell
                  (moveOneCell gridAfter2 (Crate 1) crate1Pos dir)
                  (Crate 2)
                  crate2Pos
                  dir,
                True
              )

scoreBigCrates :: Grid -> Int
scoreBigCrates grid =
  sum
    [ if isCrateAt (line, column) grid
        && (crateNumberAt (line, column) grid == 1)
        then coordinate grid (line, column)
        else 0
      | line <- [1 .. length grid - 2], -- The border of the grid is only walls -> we can skip the walls
        column <- [1 .. length (head grid) - 2]
    ]

puzzle2 :: Grid -> Movements -> Int
puzzle2 grid moves = scoreBigCrates $ applyMoves moveBigCrates grid moves (findRobot grid)

test :: IO ()
test = do
  (grid, moves) <- loadFile "test"
  let bigGrid = doubleGrid grid
  printGrid $ applyMoves moveBigCrates bigGrid moves $ findRobot bigGrid
  print $ scoreBigCrates $ applyMoves moveBigCrates bigGrid moves (findRobot bigGrid)

main :: IO ()
main =
  do
    print "Test"
    (grid, movements) <- loadFile "test"
    print (puzzle1 grid movements)
    print (puzzle2 (doubleGrid grid) movements)

    print "Input"
    (grid, movements) <- loadFile "input"
    print (puzzle1 grid movements)
    print (puzzle2 (doubleGrid grid) movements)
