import System.IO
import Data.List

getLines :: String -> IO [String]
getLines filePath =
  do
    content <- readFile filePath
    return (lines content)

parseInput :: [String] -> ([Int], [Int])
parseInput = unzip . map parseLine

parseLine :: String -> (Int, Int)
parseLine = toPair . map (read :: String -> Int) . words
  where
    toPair [a, b] = (a, b)

puzzle1 :: [Int] -> [Int] -> Int
puzzle1 left right = compute (sort left) (sort right)
  where
    compute [] [] = 0
    compute (hl : tl) (hr : tr) = distance hl hr + compute tl tr
    distance a b = abs (a - b)

puzzle2 :: [Int] -> [Int] -> Int
puzzle2 tl right
  = foldr (\ hl -> (+) (hl * length (filter (== hl) right))) 0 tl

main :: IO ()
main =
  do
    print "Test"
    lines <- getLines "test"
    let (left, right) = parseInput lines
    print (puzzle1 left right)
    print (puzzle2 left right)
    print "Input"
    lines <- getLines "input"
    let (left, right) = parseInput lines
    print (puzzle1 left right)
    print (puzzle2 left right)
