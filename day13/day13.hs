import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Debug.Trace

type Button = (Int, Int)

type Prize = (Int, Int)

type Machine = (Button, Button, Prize)

parseButton :: String -> Button
parseButton s =
  let pluses = elemIndices '+' s
      comma = fromJust $ elemIndex ',' s
      x = read $ take (comma - head pluses - 1) $ drop (head pluses + 1) s
      y = read $ drop (pluses !! 1 + 1) s
   in (x, y)

parsePrize :: Int -> String -> Prize
parsePrize toAdd s =
  let equals = elemIndices '=' s
      comma = fromJust $ elemIndex ',' s
      x = read $ take (comma - head equals - 1) $ drop (head equals + 1) s
      y = read $ drop (equals !! 1 + 1) s
   in (x + toAdd, y + toAdd)

parseFile :: Int -> [String] -> [Machine]
parseFile _ [] = []
parseFile toAdd [b1, b2, p] =
  [(parseButton b1, parseButton b2, parsePrize toAdd p)]
parseFile toAdd (b1 : b2 : p : t) =
  (parseButton b1, parseButton b2, parsePrize toAdd p)
    : parseFile toAdd (if null (head t) then tail t else t)

loadFile :: Int -> String -> IO [Machine]
loadFile toAdd file = do
  content <- readFile file
  return (parseFile toAdd (lines content))

solve :: Machine -> Maybe (Int, Int)
solve (b1, b2, target) =
  let bNum = fst b1 * snd target - snd b1 * fst target
      bDen = snd b2 * fst b1 - fst b2 * snd b1
      b = bNum `div` bDen
      aNum = fst target - fst b2 * b
      aDen = fst b1
      a = aNum `div` aDen
   in if bNum `mod` bDen /= 0 || aNum `mod` aDen /= 0
        then Nothing
        else Just (a, b)

cost :: Machine -> Int
cost m =
  let s = solve m
   in maybe 0 (\(a, b) -> 3 * a + b) s

puzzle1 :: [Machine] -> Int
puzzle1 = sum . map cost

puzzle2 :: [Machine] -> Int
puzzle2 = puzzle1

main :: IO ()
main =
  do
    print "Test"
    machines <- loadFile 0 "test"
    print (puzzle1 machines)
    machines <- loadFile 10000000000000 "test"
    print (puzzle2 machines)

    print "Input"
    machines <- loadFile 0 "input"
    print (puzzle1 machines)
    machines <- loadFile 10000000000000 "input"
    print (puzzle2 machines)
