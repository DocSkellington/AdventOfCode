import Data.List
import Data.Maybe
import Debug.Trace

type Equation = (Integer, [Integer])

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

concatNumbers :: Integer -> Integer -> Integer
concatNumbers a b = read (show a ++ show b)

data Op = Add | Mul | Concat deriving (Enum)

apply :: Op -> Integer -> Integer -> Integer
apply Add x y = x + y
apply Mul x y = x * y
apply Concat x y = concatNumbers x y

isCalibration :: [Op] -> Integer -> Equation -> Bool
isCalibration _ acc (target, []) = target == acc
isCalibration operators acc (target, numbers)
  | acc > target = False
  | otherwise =
      or
        [ isCalibration
            operators
            (apply op acc (head numbers))
            (target, tail numbers)
          | op <- operators
        ]

puzzle1 :: [Equation] -> Integer
puzzle1 equations =
  let calibrations =
        filter
          ( \(target, numbers) ->
              isCalibration [Add, Mul] (head numbers) (target, tail numbers)
          )
          equations
      evaluations = map fst calibrations
   in sum evaluations

puzzle2 :: [Equation] -> Integer
puzzle2 equations =
  let calibrations =
        filter
          ( \(target, numbers) ->
              isCalibration [Add, Mul, Concat] (head numbers) (target, tail numbers)
          )
          equations
      evaluations = map fst calibrations
   in sum evaluations

main :: IO ()
main =
  do
    print "Test"
    equations <- loadFile "test"
    print equations
    print (puzzle1 equations)
    print (puzzle2 equations)

    print "Input"
    equations <- loadFile "input"
    print (puzzle1 equations)
    print (puzzle2 equations)
