{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}
import Data.List
import Data.Maybe
import Debug.Trace
import Prelude hiding (Left, Right)

data Cell = Antenna Char | None deriving (Show)

isAntenna :: Cell -> Bool
isAntenna (Antenna _) = True
isAntenna _ = False

isNone :: Cell -> Bool
isNone = not . isAntenna

fromAntenna :: Cell -> Char
fromAntenna (Antenna a) = a

type Position = (Int, Int)

charToCell :: Char -> Cell
charToCell '.' = None
charToCell a = Antenna a

parseLine :: String -> [Cell]
parseLine = map charToCell

type Grid = [[Cell]]

outOfBounds :: Position -> Grid -> Bool
outOfBounds (x, y) grid =
  x < 0
    || x >= length grid
    || y < 0
    || y >= length (head grid)

getCell :: Position -> Grid -> Cell
getCell (x, y) grid = (grid !! x) !! y

antennaTypes :: Grid -> [Char]
antennaTypes grid =
  let allAntennaCells = concatMap (filter isAntenna) grid
      allAntennas = map fromAntenna allAntennaCells
   in nub allAntennas

positionsAntennasLine :: [Cell] -> Char -> Int -> Int -> [Position]
positionsAntennasLine [] _ _ _ = []
positionsAntennasLine (h : t) antenna x y
  | isAntenna h && fromAntenna h == antenna =
      (x, y) : positionsAntennasLine t antenna x (y + 1)
  | otherwise = positionsAntennasLine t antenna x (y + 1)

positionsAntennas :: Grid -> Char -> [Position]
positionsAntennas grid antenna =
  let getPositions [] _ = []
      getPositions (h : t) x =
        positionsAntennasLine h antenna x 0 ++ getPositions t (x + 1)
   in getPositions grid 0

distance :: Position -> Position -> Int
distance a b = abs (fst a - fst b) + abs (snd a - snd b)

direction :: Position -> Position -> Position
direction a b = (fst b - fst a, snd b - snd a)

add :: Position -> Position -> Position
add a b = (fst a + fst b, snd a + snd b)

sub :: Position -> Position -> Position
sub a b = (fst a - fst b, snd a - snd b)

pairs :: [Position] -> [(Position, Position)]
pairs [] = []
pairs (h : t) = pairs' h t ++ pairs t
  where
    pairs' :: Position -> [Position] -> [(Position, Position)]
    pairs' h [] = []
    pairs' h (h' : t) = (h, h') : pairs' h t

antinodesForPair :: Grid -> Position -> Position -> [Position]
antinodesForPair grid a b
  | a == b = []
  | otherwise =
      let possibilities = [add b (direction a b), add a (direction b a)]
       in filter (\p -> not (outOfBounds p grid)) possibilities

antinodesFor :: Grid -> [Position] -> [Position]
antinodesFor grid antennas =
  concatMap (uncurry (antinodesForPair grid)) (pairs antennas)

parseLines :: [String] -> Grid
parseLines = map parseLine

loadFile :: String -> IO Grid
loadFile file = do
  content <- readFile file
  return (parseLines (lines content))

puzzle1 :: Grid -> Int
puzzle1 grid =
  length
    ( nub
        ( concatMap
            (antinodesFor grid . positionsAntennas grid)
            (antennaTypes grid)
        )
    )

resonantAntinodesForPair :: Grid -> Position -> Position -> [Position]
resonantAntinodesForPair grid a b
  | a == b = []
  | otherwise =
      let from dir p =
            takeWhile (\p' -> not (outOfBounds p' grid)) (iterate (add dir) p)
          fromA = from (direction b a) a
          fromB = from (direction a b) b
       in filter (\p -> not (outOfBounds p grid)) (fromA ++ fromB)

resonantAntinodesFor :: Grid -> [Position] -> [Position]
resonantAntinodesFor grid antennas =
  concatMap (uncurry (resonantAntinodesForPair grid)) (pairs antennas)

puzzle2 :: Grid -> Int
puzzle2 grid =
  length
    ( nub
        ( concatMap
            (resonantAntinodesFor grid . positionsAntennas grid)
            (antennaTypes grid)
        )
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
