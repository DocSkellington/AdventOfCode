import Data.Char
import Data.Foldable
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Debug.Trace
import Prelude hiding (Left, Right)

type Position = (Int, Int)

type Garden a = [[a]]

loadFile :: String -> IO (Garden Char)
loadFile file = do
  content <- readFile file
  return (lines content)

plot :: Garden a -> Position -> a
plot garden (x, y) = (garden !! x) !! y

outOfBounds :: Garden a -> Position -> Bool
outOfBounds garden (x, y) =
  x < 0
    || x >= length garden
    || y < 0
    || y >= length (head garden)

type Regions = Map.Map Int (Set.Set Position)

-- Identifies the regions by exploring the garden in a DFS-style
findRegions :: (Eq a) => Garden a -> Regions
findRegions garden = go (0, 0) 0 Map.empty Set.empty
  where
    addToRegion :: Position -> Int -> Regions -> Regions
    addToRegion position regionId =
      Map.insertWith Set.union regionId (Set.singleton position)

    -- exploreWhileSame :: Position -> a -> Int -> Regions -> Set.Set Position -> (Regions, Set.Set Position)
    exploreWhileSame (line, column) plotSymbol plotId regions seen
      | outOfBounds garden (line, column) = (regions, seen)
      | plot garden (line, column) /= plotSymbol = (regions, seen)
      | Set.member (line, column) seen = (regions, seen)
      | otherwise =
          let (regionsAfterLeft, seenAfterLeft) =
                exploreWhileSame
                  (line, column - 1)
                  plotSymbol
                  plotId
                  (addToRegion (line, column) plotId regions)
                  (Set.insert (line, column) seen)
              (regionsAfterUp, seenAfterUp) =
                exploreWhileSame
                  (line - 1, column)
                  plotSymbol
                  plotId
                  regionsAfterLeft
                  seenAfterLeft
              (regionsAfterRight, seenAfterRight) =
                exploreWhileSame
                  (line, column + 1)
                  plotSymbol
                  plotId
                  regionsAfterUp
                  seenAfterUp
              (regionsAfterDown, seenAfterDown) =
                exploreWhileSame
                  (line + 1, column)
                  plotSymbol
                  plotId
                  regionsAfterRight
                  seenAfterRight
           in (regionsAfterDown, seenAfterDown)

    go :: Position -> Int -> Regions -> Set.Set Position -> Regions
    go (line, column) newRegion regions seen
      | line >= length garden = regions
      | column >= length (garden !! line) =
          go (line + 1, 0) newRegion regions seen
      | Set.member (line, column) seen =
          go (line, column + 1) newRegion regions seen
      | otherwise =
          let (nextRegions, nextSeen) =
                exploreWhileSame
                  (line, column)
                  (plot garden (line, column))
                  newRegion
                  regions
                  seen
           in go (line, column + 1) (newRegion + 1) nextRegions nextSeen

area :: Regions -> Int -> Int
area regions region = length $ regions Map.! region

data Fence = Up Position | Down Position | Left Position | Right Position
  deriving (Eq, Ord)

isUp :: Fence -> Bool
isUp (Up _) = True
isUp (Down _) = False
isUp (Left _) = False
isUp (Right _) = False

isDown :: Fence -> Bool
isDown (Up _) = False
isDown (Down _) = True
isDown (Left _) = False
isDown (Right _) = False

isLeft :: Fence -> Bool
isLeft (Up _) = False
isLeft (Down _) = False
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Fence -> Bool
isRight (Up _) = False
isRight (Down _) = False
isRight (Left _) = False
isRight (Right _) = True

positionOfFence :: Fence -> Position
positionOfFence (Up pos) = pos
positionOfFence (Down pos) = pos
positionOfFence (Left pos) = pos
positionOfFence (Right pos) = pos

-- Defines the needed fences
-- A fence is defined by its position in the garden, and its position inside the plot
fences :: (Eq a) => Garden a -> Regions -> Int -> [Fence]
fences garden regions region =
  let r = regions Map.! region
      needsFence :: Position -> Fence -> Bool
      needsFence pos fence
        | outOfBounds garden (positionOfFence fence) = True
        | plot garden pos /= plot garden (positionOfFence fence) = True
        | otherwise = False
      constructFences :: Position -> [Fence]
      constructFences (line, column) =
        filter
          (needsFence (line, column))
          [ Up (line + 1, column),
            Down (line - 1, column),
            Left (line, column - 1),
            Right (line, column + 1)
          ]
      go :: [Position] -> [Fence]
      go [] = []
      go (h : t) = constructFences h ++ go t
   in go (toList $ regions Map.! region)

perimeter :: (Eq a) => Garden a -> Regions -> Int -> Int
perimeter garden regions region = length $ fences garden regions region

sides :: (Eq a) => Garden a -> Regions -> Int -> Int
sides garden regions region = length $ fuseSides $ fences garden regions region
  where
    -- To determine whether a fence belongs to a side, it is sufficient to look at the currently known endpoints
    -- Either we extend the side, or the fence does not belong to it
    -- This works because the fence function adds the fences in a linear fashion!
    isInSide :: Fence -> Set.Set Fence -> Bool
    isInSide fence side
      | isUp fence || isDown fence = -- The line must be the same, and the columns must differ by at most one
          let fence' = Set.findMin side
              fence'' = Set.findMax side
              (line, column) = positionOfFence fence
              (line', column') = positionOfFence fence'
              (line'', column'') = positionOfFence fence''
          in ((isUp fence && isUp fence') || (isDown fence && isDown fence'))
                && ( line == line'
                      && line' == line''
                      && (abs (column - column') == 1 || abs (column - column'') == 1)
                  )
      | otherwise = -- Left or Right; the columns must be the same
          let fence' = Set.findMin side
              fence'' = Set.findMax side
              (line, column) = positionOfFence fence
              (line', column') = positionOfFence fence'
              (line'', column'') = positionOfFence fence''
          in ((isLeft fence && isLeft fence') || (isRight fence && isRight fence'))
                && ( (abs (line - line') == 1 || abs (line - line'') == 1)
                      && column == column'
                      && column == column''
                  )

    findSide :: Fence -> [Set.Set Fence] -> Int -> Maybe Int
    findSide _ [] _ = Nothing
    findSide position (h : t) i
      | isInSide position h = Just i
      | otherwise = findSide position t (i + 1)

    fuseSides :: [Fence] -> [Set.Set Fence]
    fuseSides [] = []
    fuseSides (h : t) =
      let rest = fuseSides t
          i = findSide h rest 0
       in if isNothing i
            then
              Set.singleton h : rest
            else
              take (fromJust i) rest
                ++ [Set.insert h (rest !! fromJust i)]
                ++ drop (fromJust i + 1) rest

puzzle1 :: Garden Char -> Int
puzzle1 garden =
  let regions = findRegions garden
   in sum (map (\region -> area regions region * perimeter garden regions region) (Map.keys regions))

puzzle2 :: Garden Char -> Int
puzzle2 garden =
  let regions = findRegions garden
   in sum (map (\region -> area regions region * sides garden regions region) (Map.keys regions))

main :: IO ()
main =
  do
    print "Test"
    garden <- loadFile "test"
    print (puzzle1 garden)
    print (puzzle2 garden)

    print "Input"
    garden <- loadFile "input"
    print (puzzle1 garden)
    print (puzzle2 garden)
