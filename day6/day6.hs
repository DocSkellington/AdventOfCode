import Data.List
import Data.Maybe
import Data.Sequence qualified as Seq
import Debug.Trace
import Prelude hiding (Left, Right)

data Cell = Crate | None deriving (Show)

isCrate :: Cell -> Bool
isCrate Crate = True
isCrate _ = False

isNone :: Cell -> Bool
isNone = not . isCrate

type Position = (Int, Int)

charToCell :: Char -> Int -> Int -> (Cell, Maybe Position)
charToCell '^' x y = (None, Just (x, y))
charToCell '#' _ _ = (Crate, Nothing)
charToCell _ _ _ = (None, Nothing)

parseLine :: String -> Int -> Int -> (Seq.Seq Cell, Maybe Position)
parseLine "" _ _ = (Seq.empty, Nothing)
parseLine (h : t) x y =
  let (cell, guardCell) = charToCell h x y
      (rest, guardRest) = parseLine t x (y + 1)
      guard = if isJust guardCell then guardCell else guardRest
   in (cell Seq.<| rest, guard)

type Map = Seq.Seq (Seq.Seq Cell)

outOfBounds :: Position -> Map -> Bool
outOfBounds (x, y) map =
  x < 0
    || x >= length map
    || y < 0
    || y >= Seq.length (Seq.index map 0)

hasCrateAt :: Position -> Map -> Bool
hasCrateAt pos map = isCrate (getCell pos map)

parseLines :: [String] -> Int -> (Map, Maybe Position)
parseLines [] _ = (Seq.empty, Nothing)
parseLines (h : t) x =
  let (line, guardLine) = parseLine h x 0
      (rest, guardRest) = parseLines t (x + 1)
      guard = if isJust guardLine then guardLine else guardRest
   in (line Seq.<| rest, guard)

loadFile :: String -> IO (Map, Position)
loadFile file = do
  content <- readFile file
  let (map, guard) = parseLines (lines content) 0
  return (map, fromJust guard)

data Dir = Up | Down | Left | Right deriving (Enum, Show, Eq, Ord)

allDirs :: [Dir]
allDirs = [Up ..]

turnRight :: Dir -> Dir
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

getCell :: Position -> Map -> Cell
getCell (x, y) map = Seq.index (Seq.index map x) y

nextPosition :: Position -> Dir -> Position
nextPosition (x, y) Up = (x - 1, y)
nextPosition (x, y) Down = (x + 1, y)
nextPosition (x, y) Left = (x, y - 1)
nextPosition (x, y) Right = (x, y + 1)

route :: Position -> Dir -> Map -> [Position] -> [Position]
route pos dir map seen
  | outOfBounds pos map = nub seen
  | otherwise =
      let nextPos = nextPosition pos dir
          newSeen = pos : seen
       in if outOfBounds nextPos map || not (hasCrateAt nextPos map)
            then
              route nextPos dir map newSeen
            else
              let newDir = turnRight dir
               in route (nextPosition pos newDir) newDir map newSeen

puzzle1 :: Map -> Position -> Int
puzzle1 map guard =
  let traversed = route guard Up map []
   in length traversed

hasCrateAt' :: Position -> Map -> Position -> Bool
hasCrateAt' pos map crate
  | pos == crate = True
  | otherwise = hasCrateAt pos map

isInLoop :: Position -> Position -> Dir -> Map -> [(Position, Dir)] -> Bool
isInLoop cratePos pos dir map seen -- seen only contains positions where the guard turned
  | outOfBounds nextPos map = False
  | otherwise =
      if not (hasCrateAt' nextPos map cratePos)
        then
          isInLoop cratePos nextPos dir map seen
        else
          ((pos, dir) `elem` seen)
            || ( let newDir = turnRight dir
                  in isInLoop cratePos pos newDir map ((pos, dir) : seen) -- We cannot move yet, as there may be an obstacle in the new direction. Funnily, this doesn't occur for part 1...
               )
  where
    nextPos = nextPosition pos dir

puzzle2 :: Map -> Position -> Int
puzzle2 grid guard =
  length
    ( filter
        (\newCrate -> (newCrate /= guard) && isInLoop newCrate guard Up grid [])
        -- (route guard Up grid [])
        [(x, y) | x <- [0 .. Seq.length grid - 1], y <- [0 .. Seq.length (Seq.index grid 0) - 1]]
    )

main :: IO ()
main =
  do
    print "Test"
    (map, guard) <- loadFile "test"
    print (puzzle1 map guard)
    print (puzzle2 map guard)

    print "Input"
    (map, guard) <- loadFile "input"
    print (puzzle1 map guard)
    print (puzzle2 map guard)
