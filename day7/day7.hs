import Data.List
import Data.Maybe
import Debug.Trace

type Equation = (Int, [Int])

parseLine :: String -> Equation
parseLine s =
  let commaIndex = fromJust (elemIndex ':' s)
      target = read (take commaIndex s)
      numbers = map read (words (drop (commaIndex + 1) s))
   in (target, numbers)

parseLines :: [String] -> [Equation]
parseLines = map parseLine

loadFile :: String -> IO [Equation]
loadFile file = do
  content <- readFile file
  return (parseLines (lines content))

concatNumbers :: Int -> Int -> Int
concatNumbers a b = a * 10 ^ floor (logBase 10 (fromIntegral b) + 1) + b

isCalibration :: [Int -> Int -> Int] -> Int -> Equation -> Bool
isCalibration _ acc (target, []) = target == acc
isCalibration operators acc (target, numbers)
  | acc > target = False
  | otherwise =
      or
        [ isCalibration
            operators
            (op acc (head numbers))
            (target, tail numbers)
          | op <- operators
        ]

puzzle1 :: [Equation] -> Int
puzzle1 equations =
  let calibrations =
        filter
          ( \(target, numbers) ->
              isCalibration [(+), (*)] (head numbers) (target, tail numbers)
          )
          equations
      evaluations = map fst calibrations
   in sum evaluations

puzzle2 :: [Equation] -> Int
puzzle2 equations =
  let calibrations =
        filter
          ( \(target, numbers) ->
              isCalibration [(+), (*), concatNumbers] (head numbers) (target, tail numbers)
          )
          equations
      evaluations = map fst calibrations
   in sum evaluations

main :: IO ()
main =
  do
    print "Test"
    equations <- loadFile "test"
    print (puzzle1 equations)
    print (puzzle2 equations)

    print "Input"
    equations <- loadFile "input"
    print (puzzle1 equations)
    print (puzzle2 equations)
