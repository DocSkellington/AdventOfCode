import Data.Char
import Data.Maybe
import Debug.Trace (trace)

type Mul = (Int, Int)

startsWithMul :: String -> Bool
startsWithMul s = take 4 s == "mul("

getNextNumber :: Bool -> String -> Maybe (Int, Int)
getNextNumber first = getNextNumber' ""
  where
    getNextNumber' num [] = Just (read num, length num)
    getNextNumber' num (h : t)
      | isDigit h = getNextNumber' (num ++ [h]) t
      | first && h == ',' = Just (read num, length num)
      | not first && h == ')' = Just (read num, length num)
      | otherwise = Nothing

getMul :: String -> Maybe Mul
getMul s
  | not (startsWithMul s) = Nothing
  | otherwise =
      let afterMul = drop 4 s
          firstNumAndLength = getNextNumber True afterMul
       in if isNothing firstNumAndLength
            then Nothing
            else
              let (firstNum, firstLength) = fromJust firstNumAndLength
                  comma = drop firstLength afterMul
               in if head comma /= ','
                    then Nothing
                    else
                      let afterComma = drop 1 comma
                          secondNumAndLength = getNextNumber False afterComma
                       in if isNothing secondNumAndLength
                            then Nothing
                            else
                              let (secondNum, secondLength) = fromJust secondNumAndLength
                                  end = drop secondLength afterComma
                               in if head end /= ')'
                                    then Nothing
                                    else Just (firstNum, secondNum)

parseLine :: String -> [Mul]
parseLine [] = []
parseLine (h : t) = if isJust mul then fromJust mul : parseLine t else parseLine t
  where
    mul = getMul (h : t)

startsWithDo :: String -> Bool
startsWithDo s = take 4 s == "do()"

startsWithDon't :: String -> Bool
startsWithDon't s = take 7 s == "don't()"

parseLineWithDoAndDon't :: String -> Bool -> [Mul]
parseLineWithDoAndDon't [] _ = []
parseLineWithDoAndDon't l False
  | startsWithDo l = parseLineWithDoAndDon't (drop 4 l) True
  | otherwise = parseLineWithDoAndDon't (tail l) False
parseLineWithDoAndDon't l True
  | startsWithDon't l = parseLineWithDoAndDon't (drop 7 l) False
  | otherwise =
      let mul = getMul l
       in if isJust mul
            then
              fromJust mul : parseLineWithDoAndDon't (tail l) True
            else
              parseLineWithDoAndDon't (tail l) True

puzzle :: [Mul] -> Int
puzzle = foldr (\(m1, m2) -> (+) (m1 * m2)) 0

main :: IO ()
main =
  do
    print "Test"
    l <- readFile "test"
    let mul = parseLine l
    print (puzzle mul)
    let mul = parseLineWithDoAndDon't l True
    print (puzzle mul)

    print "Input"
    l <- readFile "input"
    let mul = parseLine l
    print (puzzle mul)
    let mul = parseLineWithDoAndDon't l True
    print (puzzle mul)
