import Data.List
import Data.Maybe
import Prelude hiding (Left, Right)

data Dir = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
  deriving (Enum, Show)

allDirs :: [Dir]
allDirs = [Up ..]

reverseDirection :: Dir -> Dir
reverseDirection Up = Down
reverseDirection Down = Up
reverseDirection Left = Right
reverseDirection Right = Left
reverseDirection UpLeft = DownRight
reverseDirection UpRight = DownLeft
reverseDirection DownLeft = UpRight
reverseDirection DownRight = UpLeft

type Position = (Int, Int)

nextPosition :: Position -> Dir -> Position
nextPosition (x, y) Up = (x - 1, y)
nextPosition (x, y) Down = (x + 1, y)
nextPosition (x, y) Left = (x, y - 1)
nextPosition (x, y) Right = (x, y + 1)
nextPosition (x, y) UpLeft = nextPosition (nextPosition (x, y) Up) Left
nextPosition (x, y) UpRight = nextPosition (nextPosition (x, y) Up) Right
nextPosition (x, y) DownLeft = nextPosition (nextPosition (x, y) Down) Left
nextPosition (x, y) DownRight = nextPosition (nextPosition (x, y) Down) Right

type Grid = [[Char]]

getLetter :: Grid -> Position -> Char
getLetter grid pos = grid !! fst pos !! snd pos

loadFile :: String -> IO [String]
loadFile file = do
  l <- readFile file
  return (map parseLine (lines l))

parseLine :: String -> [Char]
parseLine = id

outOfBounds :: Grid -> Position -> Bool
outOfBounds grid (x, y) =
  x < 0
    || x >= length grid
    || y < 0
    || y >= length (head grid)

seekXMAS :: Grid -> Position -> Maybe Char -> Dir -> Bool
seekXMAS grid pos letter dir
  | isNothing letter = True
  | outOfBounds grid pos = False
  | getLetter grid pos /= fromJust letter = False
  | otherwise =
      seekXMAS grid (nextPosition pos dir) (nextLetter (fromJust letter)) dir
  where
    nextLetter 'X' = Just 'M'
    nextLetter 'M' = Just 'A'
    nextLetter 'A' = Just 'S'
    nextLetter 'S' = Nothing

puzzle1 :: Grid -> Int
puzzle1 grid =
  length
    ( filter
        id
        [ seekXMAS grid (x, y) (Just 'X') dir
          | x <- [0 .. length grid],
            y <- [0 .. length (head grid)],
            dir <- allDirs
        ]
    )

seekMAS :: Grid -> Position -> Maybe Char -> Dir -> Bool
seekMAS grid pos letter dir
  | isNothing letter = True
  | outOfBounds grid pos = False
  | getLetter grid pos /= fromJust letter = False
  | otherwise =
      seekMAS grid (nextPosition pos dir) (nextLetter (fromJust letter)) dir
  where
    nextLetter 'M' = Just 'A'
    nextLetter 'A' = Just 'S'
    nextLetter 'S' = Nothing

seekX_MAS :: Grid -> Position -> Bool
seekX_MAS grid pos
  | getLetter grid pos /= 'A' = False
  | otherwise =
      length
        ( filter
            id
            [ seekMAS grid (nextPosition pos dir) firstLetter (reverseDirection dir)
              | dir <- [UpRight, UpLeft, DownRight, DownLeft]
            ]
        )
        >= 2
  where
    firstLetter = Just 'M'

puzzle2 :: Grid -> Int
puzzle2 grid =
  length
    ( filter
        (/= (-1, -1))
        [ if seekX_MAS grid (x, y) then (x, y) else (-1, -1)
          | x <- [0 .. length grid - 1],
            y <- [0 .. length (head grid) - 1]
        ]
    )

main :: IO ()
main =
  do
    print "Test"
    grid <- loadFile "test"
    print (puzzle1 grid)
    print (puzzle2 grid)

    print "Input"
    grid <- loadFile "input"
    print (puzzle1 grid)
    print (puzzle2 grid)
