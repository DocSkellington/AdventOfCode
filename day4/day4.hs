-- cabal repl --build-depends pureMD5
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Data.String
import Distribution.Utils.MD5

hash :: String -> Int -> String
hash s n = show (md5 (fromString (s ++ show n)))

startsWithZeros :: Int -> String -> Int -> Bool
startsWithZeros zeros s n =
  let hashed = hash s n
   in take zeros hashed == replicate zeros '0'

puzzle1 :: String -> Int
puzzle1 s =
  fromJust
    ( find
        (startsWithZeros 5 s)
        [0 ..]
    )

puzzle2 :: String -> Int
puzzle2 s =
  fromJust
    ( find
        (startsWithZeros 6 s)
        [0 ..]
    )

main :: IO ()
main =
  do
    print "Test"
    l <- readFile "test"
    print (puzzle1 l)

    print "Input"
    l <- readFile "input"
    print (puzzle1 l)
    print (puzzle2 l)
