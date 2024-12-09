import Data.Char
import Data.Maybe
import Data.Sequence qualified as Seq
import Debug.Trace
import Prelude hiding (Left, Right)

data Layout = File Int Int | Free Int deriving (Show)

isFile :: Layout -> Bool
isFile (File _ _) = True
isFile (Free _) = False

isFree :: Layout -> Bool
isFree = not . isFile

size :: Layout -> Int
size (File _ a) = a
size (Free a) = a

fileId :: Layout -> Int
fileId (File id _) = id

type DiskMap = Seq.Seq Layout

parseLine :: String -> Int -> DiskMap
parseLine "" _ = Seq.empty
parseLine [file] fileId = Seq.singleton (File fileId (digitToInt file))
parseLine (file : free : t) fileId =
  File fileId (digitToInt file)
    Seq.<| Free (digitToInt free)
    Seq.<| parseLine t (fileId + 1)

loadFile :: String -> IO DiskMap
loadFile file = do
  content <- readFile file
  return (parseLine (head (lines content)) 0)

type DiskContent = Seq.Seq Int

toContent :: Layout -> DiskContent
toContent (File id size) = Seq.replicate size id
toContent (Free size) = Seq.replicate size (-1)

mapToContent :: DiskMap -> DiskContent
mapToContent = mapToContent' 0
  where
    mapToContent' index disk
      | index >= length disk = Seq.empty
      | otherwise =
          toContent (Seq.index disk index)
            Seq.>< mapToContent' (index + 1) disk

-- This could be re-implemented in the same way as compactWhole: create a new DiskMap with the fragmented files... But this version already works :)
compact :: DiskContent -> Int -> Int -> DiskContent
compact diskMap indexHead indexTail
  | indexHead > indexTail = Seq.empty
  | Seq.index diskMap indexHead /= -1 =
      Seq.index diskMap indexHead
        Seq.<| compact diskMap (indexHead + 1) indexTail
  | otherwise =
      let nextIndexTail index
            | indexHead > index = index
            | Seq.index diskMap index == -1 = nextIndexTail (index - 1)
            | otherwise = index
       in Seq.index diskMap indexTail
            Seq.<| compact diskMap (indexHead + 1) (nextIndexTail (indexTail - 1))

checksum :: DiskContent -> Int
checksum =
  Seq.foldlWithIndex
    (\old index new -> if new == -1 then old else old + index * new)
    0

puzzle1 :: DiskMap -> Int
puzzle1 disk =
  let content = mapToContent disk
   in checksum (compact content 0 (length content - 1))

-- This could be improved by maintaining a map of "free space size" -> indices in the DiskMap. I'm okay with a solution taking 6s, though
seekSpace :: DiskMap -> Layout -> Int -> Maybe Int
seekSpace disk file indexFile = seekSpace' disk file indexFile 0
  where
    seekSpace' :: DiskMap -> Layout -> Int -> Int -> Maybe Int
    seekSpace' disk file indexFile index
      | index >= indexFile || index >= length disk = Nothing
      | isFree (Seq.index disk index)
          && size (Seq.index disk index) >= size file =
          Just index
      | otherwise = seekSpace' disk file indexFile (index + 1)

mergeFree :: Int -> DiskMap -> DiskMap
mergeFree index disk
  | index > length disk = disk
  | not (isFree (Seq.index disk index)) = disk
  | isFree (Seq.index disk (index - 1)) = -- Merge with the one before
      mergeFree
        (index - 1)
        ( Seq.update
            (index - 1)
            (createFree index (index - 1))
            (Seq.deleteAt index disk)
        )
  | (index + 1) < length disk && isFree (Seq.index disk (index + 1)) = -- Merge with the one after
      mergeFree
        index
        ( Seq.update
            index
            (createFree index (index + 1))
            (Seq.deleteAt index disk)
        )
  | otherwise = disk
  where
    createFree i i' = Free (size (Seq.index disk i) + size (Seq.index disk i'))

removeFileBySpace :: DiskMap -> Int -> Int -> DiskMap
removeFileBySpace disk index size =
  mergeFree index (Seq.update index (Free size) disk)

fillSpace :: DiskMap -> Int -> Int -> Layout -> DiskMap
fillSpace disk indexFree indexFile file -- In both cases, we must also replace the current file by a free space
  | size file == size (Seq.index disk indexFree) = -- Replace the free space by the file
      Seq.update
        indexFree
        file
        (removeFileBySpace disk indexFile (size file))
  | otherwise = -- "Cut" the free space in two parts
      let freeSpace = size (Seq.index disk indexFree) - size file
       in Seq.insertAt
            indexFree
            file
            ( mergeFree
                indexFree
                ( Seq.update
                    indexFree
                    (Free freeSpace)
                    (removeFileBySpace disk indexFile (size file))
                )
            )

compactWhole :: DiskMap -> DiskMap
compactWhole disk = compactWhole' (length disk - 1) disk
  where
    compactWhole' indexTail disk
      | indexTail < 0 = disk
      | isFree (Seq.index disk indexTail) = compactWhole' (indexTail - 1) disk
      | otherwise =
          let file = Seq.index disk indexTail
              indexToFill = seekSpace disk file indexTail
           in if isNothing indexToFill || fromJust indexToFill >= indexTail
                then compactWhole' (indexTail - 1) disk
                else
                  compactWhole'
                    (indexTail - 1)
                    (fillSpace disk (fromJust indexToFill) indexTail file)

puzzle2 :: DiskMap -> Int
puzzle2 = checksum . mapToContent . compactWhole

main :: IO ()
main =
  do
    print "Test"
    disk <- loadFile "test"
    print (puzzle1 disk)
    print (puzzle2 disk)

    print "Input"
    disk <- loadFile "input"
    print (puzzle1 disk)
    print (puzzle2 disk)
