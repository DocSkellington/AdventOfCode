action :: Char -> Int
action h = if h == '(' then 1 else -1

puzzle1 :: String -> Int
puzzle1 = foldr ((+) . action) 0

puzzle2 :: String -> Int -> Int
puzzle2 [] _ = 0
puzzle2 (h : t) level
  | action h + level < 0 = 1
  | otherwise = 1 + puzzle2 t (level + action h)

main :: IO ()
main =
  do
    print "Test"
    line <- readFile "test"
    print (puzzle1 line)
    print (puzzle2 line 0)

    print "Input"
    line <- readFile "input"
    print (puzzle1 line)
    print (puzzle2 line 0)

