type Level = Int

type Report = [Level]

parseLine :: String -> Report
parseLine = map read . words

isGoodDistance :: Level -> Level -> Bool -> Bool
isGoodDistance r1 r2 direction = lower <= d && d <= upper
  where
    lower = if direction then -3 else 1
    upper = if direction then -1 else 3
    d = r1 - r2

isSafe :: Report -> Bool -> Bool
isSafe [_] _ = True
isSafe (h1 : h2 : t) direction = isGoodDistance h1 h2 direction && isSafe (h2 : t) direction

puzzle1 :: [Report] -> Int
puzzle1 = length . filter isSafe'
  where
    isSafe' (h1 : h2 : t) = isSafe (h1 : h2 : t) (h1 < h2)

removeOneOrNone :: Report -> [Report]
removeOneOrNone [] = [[]]
removeOneOrNone (h : t) = t : map (h :) (removeOneOrNone t)

puzzle2 :: [Report] -> Int
puzzle2 = length . filter (any isSafe' . removeOneOrNone)
  where
    isSafe' (h1 : h2 : t)
      | isSafe (h1 : h2 : t) (h1 < h2) = True
      | otherwise = False


main :: IO ()
main =
  do
    print "Test"
    l <- readFile "test"
    let reports = map parseLine (lines l)
    print (puzzle1 reports)
    print (puzzle2 reports)

    print "Input"
    l <- readFile "input"
    let reports = map parseLine (lines l)
    print (puzzle1 reports)
    print (puzzle2 reports)
