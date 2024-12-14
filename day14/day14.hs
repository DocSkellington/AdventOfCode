import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Debug.Trace
import System.IO
import Control.Monad

type Position = (Int, Int)

type Velocity = (Int, Int)

type Robot = (Position, Velocity)

parseLine :: String -> Robot
parseLine s =
  let equals = elemIndices '=' $ s
      commas = elemIndices ',' s
      space = fromJust $ elemIndex ' ' s
      pCol = read $ take (head commas - head equals - 1) $ drop (head equals + 1) s
      pLine = read $ take (space - head commas - 1) $ drop (head commas + 1) s
      vCol = read $ take ((commas !! 1) - (equals !! 1) - 1) $ drop ((equals !! 1) + 1) s
      vLine = read $ drop ((commas !! 1) + 1) s
   in ((pLine, pCol), (vLine, vCol))

loadFile :: String -> IO [Robot]
loadFile file = do
  content <- readFile file
  return (map parseLine (lines content))

nmod :: Int -> Int -> Int
nmod a b
  | a < 0 = (b - ((-a) `mod` b)) `mod` b
  | otherwise = a `mod` b

finalPosition :: Int -> (Int, Int) -> Robot -> Position
finalPosition steps (gLine, gCol) ((pLine, pCol), (vLine, vCol)) =
  ((pLine + steps * vLine) `nmod` gLine, (pCol + steps * vCol) `nmod` gCol)

perQuadrant :: (Int, Int) -> [Position] -> [Int]
perQuadrant (gLine, gCol) = go [0, 0, 0, 0]
  where
    lineMiddle :: Int
    lineMiddle = gLine `div` 2

    columnMiddle :: Int
    columnMiddle = gCol `div` 2

    go :: [Int] -> [Position] -> [Int]
    go q [] = q
    go [q1, q2, q3, q4] ((line, column) : t)
      | line == lineMiddle || column == columnMiddle = go [q1, q2, q3, q4] t
      | line < lineMiddle && column < columnMiddle = go [q1 + 1, q2, q3, q4] t
      | line < lineMiddle && column > columnMiddle = go [q1, q2 + 1, q3, q4] t
      | line > lineMiddle && column < columnMiddle = go [q1, q2, q3 + 1, q4] t
      | line > lineMiddle && column > columnMiddle = go [q1, q2, q3, q4 + 1] t

safetyFactor :: Int -> (Int, Int) -> [Robot] -> Int
safetyFactor steps grid = product . perQuadrant grid . map (finalPosition steps grid)

puzzle1 :: (Int, Int) -> [Robot] -> Int
puzzle1 = safetyFactor 100

gridString :: (Int, Int) -> [Position] -> String
gridString (gLine, gCol) positions = go (0, 0)
  where
    go :: Position -> String
    go (line, column)
      | line > gLine = ""
      | column > gCol = "\n" ++ go (line + 1, 0)
      | (line, column) `elem` positions = "🟩" ++ go (line, column + 1)
      | otherwise = "⬛" ++ go (line, column + 1)

printRobots :: Int -> (Int, Int) -> [Position] -> IO ()
printRobots step grid positions = do
  putStr "Current:"
  print step
  putStrLn (gridString grid positions)

puzzle2 :: (Int, Int) -> [Robot] -> IO ()
puzzle2 grid robots = go 0
  where
    go :: Int -> IO ()
    go step =
      let safety = safetyFactor step grid robots
       in do
      -- Since the robots have to be quite close to each other, I expect the safety factor to be relatively small (compared to a random position, such as in puzzle 1); in particular, I expect many of them to lie within the dead zone
      -- So, I only print some of the positions, not all of them. Still need to go through them one by one, though...
      when (50000000 <= safety && safety <= 100000000) $ printRobots step grid (map (finalPosition step grid) robots)
      go (step + 1)

main :: IO ()
main =
  do
    print "Test"
    machines <- loadFile "test"
    let grid = (7, 11) -- Coordinates are swapped compared to the statement
    print (puzzle1 grid machines)

    print "Input"
    machines <- loadFile "input"
    let grid = (103, 101)
    print (puzzle1 grid machines)
    puzzle2 grid machines
