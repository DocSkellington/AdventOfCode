import Data.List

type Present = (Int, Int, Int)

parseLine :: String -> Present
parseLine lwh = (read l, read w, read h)
  where
    (l, wh) = span (/= 'x') lwh
    (w, h') = span (/= 'x') (dropWhile (== 'x') wh)
    h = dropWhile (== 'x') h'

puzzle1 :: [Present] -> Int
puzzle1 = foldr ((+) . compute) 0
  where
    compute (l, w, h) = 2 * lw + 2 * wh + 2 * hl + min (min lw wh) hl
      where
        lw = l * w
        wh = w * h
        hl = h * l

puzzle2 :: [Present] -> Int
puzzle2 = foldr ((+) . compute) 0
  where
    compute (l, w, h) = perimeter + l * w * h
      where
        sorted = sort [l, w, h]
        perimeter = 2 * head sorted + 2 * sorted !! 1

main :: IO ()
main =
  do
    print "Test"
    l <- readFile "test"
    let presents = map parseLine (lines l)
    print (puzzle1 presents)
    print (puzzle2 presents)

    print "Input"
    l <- readFile "input"
    let presents = map parseLine (lines l)
    print (puzzle1 presents)
    print (puzzle2 presents)
