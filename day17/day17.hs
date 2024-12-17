import Data.Bits
import Data.Char
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Debug.Trace
import System.IO
import Prelude hiding (Left, Right)

data Opcode
  = Adv Int
  | Bxl Int
  | Bst Int
  | Jnz Int
  | Bxc Int
  | Out Int
  | Bdv Int
  | Cdv Int
  deriving (Show, Eq)

type Registers = (Int, Int, Int)

type InstPointer = Int

toOpcode :: Int -> Int -> Opcode
toOpcode 0 = Adv
toOpcode 1 = Bxl
toOpcode 2 = Bst
toOpcode 3 = Jnz
toOpcode 4 = Bxc
toOpcode 5 = Out
toOpcode 6 = Bdv
toOpcode 7 = Cdv

parseRegister :: String -> Int
parseRegister s =
  let colon = fromJust $ elemIndex ':' s
   in read $ drop (colon + 2) s

intsToOpcodes :: [Int] -> [Opcode]
intsToOpcodes [] = []
intsToOpcodes [h] = []
intsToOpcodes (opcode : operand : t) =
  toOpcode opcode operand : intsToOpcodes t

retrieveNumbersProgram :: String -> [Int]
retrieveNumbersProgram s = numbers
  where
    colon = fromJust $ elemIndex ':' s
    numbers = map digitToInt $ filter (/= ',') $ drop (colon + 2) s

parseProgram :: String -> [Opcode]
parseProgram = intsToOpcodes . retrieveNumbersProgram

loadFile :: String -> IO (Registers, [Opcode])
loadFile file = do
  content <- readFile file
  let l = lines content
  let endRegister = fromJust $ elemIndex [] l
  let [a, b, c] = map parseRegister $ take endRegister l
  return ((a, b, c), parseProgram $ l !! max 0 (endRegister + 1))

divSquare :: Int -> Int -> Int
divSquare x y = truncate (fromIntegral x / (2 ** fromIntegral y))

execute :: InstPointer -> Registers -> Opcode -> (InstPointer, Registers, Maybe Int)
execute ip (a, b, c) (Adv o)
  | o <= 3 =
      (ip + 1, (divSquare a o, b, c), Nothing)
  | o == 4 =
      (ip + 1, (divSquare a a, b, c), Nothing)
  | o == 5 =
      (ip + 1, (divSquare a b, b, c), Nothing)
  | o == 6 =
      (ip + 1, (divSquare a c, b, c), Nothing)
  | o == 7 = error "7 cannot appear"
execute ip (a, b, c) (Bxl o) = (ip + 1, (a, b `xor` o, c), Nothing)
execute ip (a, b, c) (Bst o)
  | o <= 3 = (ip + 1, (a, o `mod` 8, c), Nothing)
  | o == 4 = (ip + 1, (a, a `mod` 8, c), Nothing)
  | o == 5 = (ip + 1, (a, b `mod` 8, c), Nothing)
  | o == 6 = (ip + 1, (a, c `mod` 8, c), Nothing)
  | o == 7 = error "7 cannot appear"
execute ip (a, b, c) (Jnz o)
  | a == 0 = (ip + 1, (a, b, c), Nothing)
  | otherwise = (o, (a, b, c), Nothing)
execute ip (a, b, c) (Bxc _) = (ip + 1, (a, b `xor` c, c), Nothing)
execute ip (a, b, c) (Out o)
  | o <= 3 = (ip + 1, (a, b, c), Just (o `mod` 8))
  | o == 4 = (ip + 1, (a, b, c), Just (a `mod` 8))
  | o == 5 = (ip + 1, (a, b, c), Just (b `mod` 8))
  | o == 6 = (ip + 1, (a, b, c), Just (c `mod` 8))
  | o == 7 = error "7 cannot appear"
execute ip (a, b, c) (Bdv o) =
  let (ip', (a', _, _), out') = execute ip (a, b, c) (Adv o)
   in (ip', (a, a', c), out')
execute ip (a, b, c) (Cdv o) =
  let (ip', (a', _, _), out') = execute ip (a, b, c) (Adv o)
   in (ip', (a, b, a'), out')

run :: Registers -> [Opcode] -> [Int]
run registers opcodes = go registers 0
  where
    go :: Registers -> Int -> [Int]
    go registers ip
      | ip >= length opcodes = []
      | otherwise =
          let (ip', registers', out') = execute ip registers (opcodes !! ip)
           in if isJust out'
                then
                  fromJust out' : go registers' ip'
                else go registers' ip'

puzzle1 :: Registers -> [Opcode] -> [Int]
puzzle1 = run

toInt :: [Int] -> Int
toInt [h] = h
toInt (h : t) = 8 * toInt t + h

try :: [Int] -> [Opcode] -> Int -> Bool
try targets program a = run (a, 0, 0) program == targets

-- The idea is to build the register value starting from the right-most digit
-- Computing for that last digit is easy: just try every number between 0 and 7 (as outputs are always modulo 8)
-- Once we have found one possible number (name it x_1), we keep it in memory and go to its left digit
-- Now, it gets trickier: we again try every number in [0 .. 7] but we add the x_1 digit after it. This yields a number against which we execute the program
-- We repeat this until we have found a complete number

-- Note that the only operation that requires us to build the full number is adv. Without it, the program performs every step modulo 8 and we could have just tried each digit independently from the ones following it
-- Second note: I reverse-engineered the program to obtain a simple arithmetic expression but decided to reuse the run function in the end. That way, the program can maybe be used against different inputs... Under the assumption that A is shifted three digits to the right each iteration of the program (via an adv opcode)
brute :: [Opcode] -> [Int] -> [Int]
brute program targets = go (length targets - 1) []
  where
    go :: Int -> [Int] -> [Int]
    go index currentNumber
      | index < 0 = currentNumber
      | otherwise = tryPossibilities possibilities
      where
        possibilities =
          filter
            (try (drop index targets) program . toInt)
            [x : currentNumber | x <- [0 .. 7]]

        tryPossibilities :: [[Int]] -> [Int]
        tryPossibilities [] = []
        tryPossibilities (possibility : t) =
          let answer = go (index - 1) possibility
           in if null answer
                then tryPossibilities t
                else answer

puzzle2 :: [Opcode] -> [Int] -> Int
puzzle2 program targets = toInt $ brute program targets

loadTargets :: String -> IO [Int]
loadTargets file = do
  content <- readFile file
  let l = lines content
  let endRegister = fromJust $ elemIndex [] l
  return (retrieveNumbersProgram $ l !! (endRegister + 1))

main :: IO ()
main =
  do
    print "Test"
    (registers, program) <- loadFile "test"
    print (puzzle1 registers program)

    print "Input"
    (registers, program) <- loadFile "input"
    print (puzzle1 registers program)
    targets <- loadTargets "input"
    print (puzzle2 program targets)
