import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Debug.Trace

type Rules = Map.Map Int [Int]

parseRule :: String -> (Int, Int)
parseRule s =
  let line = fromJust (elemIndex '|' s)
      (first, second) = splitAt line s
   in (read first, read (tail second))

parseRules :: [String] -> Map.Map Int [Int]
parseRules [] = Map.empty
parseRules (h : t) =
  let map = parseRules t
      (r1, r2) = parseRule h
   in Map.insertWith (++) r1 [r2] map

getRule :: Int -> Rules -> [Int]
getRule = Map.findWithDefault []

isBefore :: Int -> Int -> Rules -> Ordering
isBefore pBefore pAfter rules 
  | pBefore == pAfter = EQ
  | pAfter `elem` getRule pBefore rules = LT
  | otherwise = GT

type Print = [Int]

parsePrint :: String -> Print
parsePrint s
  | s == "" = []
  | isNothing nextComma = [read s]
  | otherwise =
      let (number, t) = splitAt (fromJust nextComma) s
       in (read number : parsePrint (tail t))
  where
    nextComma = elemIndex ',' s

parsePrints :: [String] -> [Print]
parsePrints = map parsePrint

loadFile :: String -> IO (Rules, [Print])
loadFile file = do
  content <- readFile file
  let l = lines content
  let (rules, prints) = splitAt (fromJust (elemIndex "" l)) l
  return (parseRules rules, parsePrints (tail prints))

isValidPrint :: Print -> Rules -> Print -> Bool
isValidPrint [] _ _ = True
isValidPrint (h : t) rules seen =
  let ruleForH = getRule h rules
   in not (any (`elem` ruleForH) seen) && isValidPrint t rules (h : seen)

getMiddle :: Print -> Int
getMiddle l = l !! (length l `div` 2)

puzzle1 :: Rules -> [Print] -> Int
puzzle1 rules prints =
  let validPrints = filter (\p -> isValidPrint p rules []) prints
   in foldl (\old new -> old + getMiddle new) 0 validPrints

puzzle2 :: Rules -> [Print] -> Int
puzzle2 rules prints =
  let invalidPrints = filter (\p -> not (isValidPrint p rules [])) prints
      sortedPrints = map (sortBy (\p1 p2 -> isBefore p1 p2 rules)) invalidPrints
   in foldl (\old new -> old + getMiddle new) 0 sortedPrints

main :: IO ()
main =
  do
    print "Test"
    (rules, prints) <- loadFile "test"
    print (puzzle1 rules prints)
    print (puzzle2 rules prints)

    print "Input"
    (rules, prints) <- loadFile "input"
    print (puzzle1 rules prints)
    print (puzzle2 rules prints)
