import Data.Map.Strict qualified as Map

data Move = North | South | East | West deriving (Show)

type Coordinate = (Int, Int)

type Grid = Map.Map Coordinate Bool

toMove :: Char -> Move
toMove '^' = North
toMove 'v' = South
toMove '>' = East
toMove '<' = West

parseLine :: String -> [Move]
parseLine = map toMove

applyMove :: Coordinate -> Move -> Coordinate
applyMove (x, y) North = (x, y + 1)
applyMove (x, y) South = (x, y - 1)
applyMove (x, y) East = (x + 1, y)
applyMove (x, y) West = (x - 1, y)

applyMoves :: Grid -> Coordinate -> [Move] -> Grid
applyMoves grid _ [] = grid
applyMoves grid santa (move : t) =
  applyMoves (Map.insert nextPos True grid) nextPos t
  where
    nextPos = applyMove santa move

countHouses :: Grid -> Int
countHouses = Map.size

puzzle1 :: [Move] -> Int
puzzle1 = countHouses . applyMoves (Map.singleton (0, 0) True) (0, 0)

applyMovesWithRobot :: Grid -> Coordinate -> Coordinate -> [Move] -> Grid
applyMovesWithRobot grid _ _ [] = grid
applyMovesWithRobot grid santa robot (moveSanta : moveRobot : t) =
  applyMovesWithRobot
    (Map.insert nextRobot True (Map.insert nextSanta True grid))
    nextSanta
    nextRobot
    t
  where
    nextSanta = applyMove santa moveSanta
    nextRobot = applyMove robot moveRobot

puzzle2 :: [Move] -> Int
puzzle2 = countHouses . applyMovesWithRobot (Map.singleton (0, 0) True) (0, 0) (0, 0)

main :: IO ()
main =
  do
    print "Test"
    l <- readFile "test"
    let moves = parseLine l
    print (puzzle1 moves)
    print (puzzle2 moves)

    print "Input"
    l <- readFile "input"
    let moves = parseLine l
    print (puzzle1 moves)
    print (puzzle2 moves)
