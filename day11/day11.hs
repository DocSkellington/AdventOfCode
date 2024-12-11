import Data.List
import Data.Map.Strict qualified as Map
import Debug.Trace

type Stone = Int

type Stones = [Int]

parseLine :: String -> Stones
parseLine = map read . words

loadFile :: String -> IO Stones
loadFile file = do
  content <- readFile file
  return (parseLine (head (lines content)))

numberDigits :: Stone -> Int
numberDigits stone = 1 + floor (logBase 10 $ fromIntegral stone)

evenLength :: Stone -> Bool
evenLength = even . numberDigits

split :: Stone -> [Stone]
split stone =
  let nDigits = numberDigits stone `div` 2
   in [stone `div` (10 ^ nDigits), stone `mod` (10 ^ nDigits)]

type Memo = Map.Map (Stone, Int) Int -- Stone value -> number of resulting stones

blink :: Stone -> [Stone]
blink stone
  | stone == 0 = [1]
  | evenLength stone = split stone
  | otherwise = [stone * 2024]

afterNBlinks :: Memo -> Int -> Stone -> (Int, Memo)
afterNBlinks memo n stone
  | n == 0 = (1, memo)
  | Map.member (stone, n) memo = (memo Map.! (stone, n), memo)
  | otherwise =
      let nextStones = blink stone
       in if length nextStones == 1 -- That is pretty ugly...
            then
              let (nStones, m) = afterNBlinks memo (n - 1) (head nextStones)
               in (nStones, Map.insert (stone, n) nStones m)
            else
              let (nStones, m) = afterNBlinks memo (n - 1) (head nextStones)
                  (nStones', m') = afterNBlinks m (n - 1) (head (tail nextStones))
               in (nStones + nStones', Map.insert (stone, n) (nStones + nStones') m')

compute :: Int -> Stones -> Int
compute = go Map.empty 0
  where
    go :: Memo -> Int -> Int -> Stones -> Int
    go memo acc n [] = acc
    go memo acc n (h : t) =
      let (nStones, m) = afterNBlinks memo n h
       in go m (acc + nStones) n t

puzzle1 :: Stones -> Int
puzzle1 = compute 25

puzzle2 :: Stones -> Int
puzzle2 = compute 75

main :: IO ()
main =
  do
    print "Test"
    stones <- loadFile "test"
    print (puzzle1 stones)
    print (puzzle2 stones)

    print "Input"
    stones <- loadFile "input"
    print (puzzle1 stones)
    print (puzzle2 stones)
