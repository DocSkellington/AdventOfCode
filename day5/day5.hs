-- cabal repl --build-depends pureMD5

import Data.List
import Data.Sequence qualified as Seq
import Debug.Trace

type Names = [String]

parseFile :: String -> IO Names
parseFile file =
  do
    content <- readFile file
    return (lines content)

isNice :: String -> Bool
isNice s =
  length (filter (`elem` "aeiou") s) >= 3
    && any (\i -> (s !! i) == (s !! (i + 1))) [0 .. length s - 2]
    && and [not (x `isInfixOf` s) | x <- ["ab", "cd", "pq", "xy"]]

puzzle1 :: Names -> Int
puzzle1 s = length $ filter isNice s

isNice' :: String -> Bool
isNice' s =
  any
    ( \i ->
        any -- Is there a j > i+1 such that the pattern at s(i, i+1) repeats at s(j, j+1)?
          (\j -> (s !! j) == (s !! i) && (s !! (j + 1)) == (s !! (i + 1)))
          [i + 2 .. length s - 2]
    )
    [0 .. length s - 2]
    && any
      (\i -> (s !! i) == (s !! (i + 2)) && (s !! i) /= (s !! (i + 1)))
      [0 .. length s - 3]

puzzle2 :: Names -> Int
puzzle2 s = length $ filter isNice' s

main :: IO ()
main =
  do
    print "Test"
    let l = ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"]
    print (puzzle1 l)
    let l = ["qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy"]
    print (puzzle2 l)

    print "Input"
    l <- parseFile "input"
    print (puzzle1 l)
    print (puzzle2 l)
