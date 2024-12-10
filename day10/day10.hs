import Data.Char
import Data.List
import Data.Sequence qualified as Seq
import Debug.Trace
import Prelude hiding (Left, Right)

type TopographicMap = Seq.Seq (Seq.Seq Int)

parseLine :: String -> Seq.Seq Int
parseLine = Seq.fromList . map digitToInt

loadFile :: String -> IO TopographicMap
loadFile file = do
  content <- readFile file
  return (Seq.fromList (map parseLine (lines content)))

outOfBounds :: TopographicMap -> Position -> Bool
outOfBounds topo (x, y) =
  x < 0
    || x >= length topo
    || y < 0
    || y >= length (Seq.index topo 0)

height :: Position -> TopographicMap -> Int
height pos topo
  | outOfBounds topo pos = -1
  | otherwise = Seq.index (Seq.index topo (fst pos)) (snd pos)

type Position = (Int, Int)

adjacent :: Position -> TopographicMap -> [Position]
adjacent pos topo =
  filter
    (\p -> height p topo == (height pos topo + 1)) -- New node increases height by 1
    [ (fst pos + 1, snd pos),
      (fst pos - 1, snd pos),
      (fst pos, snd pos + 1),
      (fst pos, snd pos - 1)
    ]

-- Assuming we start at 0
reachableFrom :: TopographicMap -> Position -> [Position]
reachableFrom topo pos
  | height pos topo == 9 = [pos]
  | otherwise = concatMap (reachableFrom topo) (adjacent pos topo)

trailheadScore :: TopographicMap -> Position -> Int
trailheadScore topo pos
  | height pos topo /= 0 = 0
  | otherwise = length $ nub $ reachableFrom topo pos

puzzle1 :: TopographicMap -> Int
puzzle1 topo =
  sum
    ( [ trailheadScore topo (x, y)
        | x <- [0 .. length topo],
          y <- [0 .. length (Seq.index topo 0)]
      ]
    )

trailheadRating :: TopographicMap -> Position -> Int
trailheadRating topo pos
  | height pos topo /= 0 = 0
  | otherwise = length $ reachableFrom topo pos

puzzle2 :: TopographicMap -> Int
puzzle2 topo =
  sum
    ( [ trailheadRating topo (x, y)
        | x <- [0 .. length topo],
          y <- [0 .. length (Seq.index topo 0)]
      ]
    )

main :: IO ()
main =
  do
    print "Test"
    disk <- loadFile "test"
    print (puzzle1 disk)
    print (puzzle2 disk)

    print "Input"
    disk <- loadFile "input"
    print (puzzle1 disk)
    print (puzzle2 disk)
