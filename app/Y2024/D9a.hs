{-# LANGUAGE QuasiQuotes #-}

module Y2024.D9a (run) where

import Control.Exception (throw)
import Control.Monad (forM)
import Coordinate ()
import Data.List (intersperse, sortOn)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as Vector
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Stats (GCDetails (gcdetails_par_balanced_copied_bytes))
import Helpers
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Line
  = Line
  {}
  deriving (Show)

data State
  = State
  {}
  deriving (Show, Eq, Ord)

type ID = Integer

data Cell
  = File FileDetails
  | FreeSpace Integer
  deriving (Show)

replicateCell :: Cell -> [Cell]
replicateCell cell =
  case cell of
    File FileDetails {size} -> replicate (fromInteger size) cell
    FreeSpace n -> replicate (fromInteger n) cell

data FileDetails = FileDetails
  { id :: ID,
    size :: Integer
  }
  deriving (Show)

parseLine :: String -> [Cell]
parseLine line =
  reverse $
    snd $
      foldl
        ( \(n, list) (i, c) ->
            let a = read [c]
             in if even i
                  then
                    (n + 1, File (FileDetails n a) : list)
                  else
                    ( n,
                      if a == 0 then list else FreeSpace a : list
                    )
        )
        (0 :: Integer, [])
        (zip [0 ..] line)

isFile :: Cell -> Bool
isFile (File _) = True
isFile _ = False

isFreeSpace :: Cell -> Bool
isFreeSpace (FreeSpace _) = True
isFreeSpace _ = False

-- This time, attempt to move whole files to the leftmost span of free space blocks that
-- could fit the file. Attempt to move each file exactly once in order of decreasing file
-- ID number starting with the file with the highest file ID number. If there is no span of
-- free space to the left of a file that is large enough to fit the file, the file does not
-- move.
-- 1. Find the first free space block.
-- 2. Find the file with the highest ID number that is to the right of the free space block.
move :: Vector.Vector Cell -> Vector.Vector Cell
move s =
  move_ Set.empty s

move_ movedFiles s =
  let moveableFiles :: [(Int, FileDetails)] =
        [0 .. length s - 1]
          |> map
            ( \i ->
                case s Vector.! i of
                  File details@FileDetails {id} ->
                    if Set.member id movedFiles
                      then
                        Nothing
                      else
                        Just (i, details)
                  _ ->
                    Nothing
            )
          |> catMaybes
          |> sortOn (\(_, FileDetails {id}) -> -id)
   in if null moveableFiles
        then
          s
        else
          -- Find the first free space block
          let (fileIndex, FileDetails {id, size}) = head moveableFiles
              blocks =
                [0 .. fileIndex]
                  |> map
                    ( \i ->
                        case s Vector.! i of
                          FreeSpace freeBlockSize ->
                            if freeBlockSize >= size
                              then
                                Just (i, freeBlockSize)
                              else
                                Nothing
                          _ ->
                            Nothing
                    )
                  |> catMaybes
           in move_
                (Set.insert id movedFiles)
                $ if null blocks
                  then
                    s
                  else
                    let (firstOpenBlockIndex, blockSize) = head blocks
                     in if blockSize == size
                          then
                            let swapped =
                                  s
                                    Vector.// [ (firstOpenBlockIndex, s Vector.! fileIndex),
                                                (fileIndex, s Vector.! firstOpenBlockIndex)
                                              ]
                             in -- !z = traceShowId $ map toId $ Vector.toList swapped
                                swapped
                          else
                            let diff = blockSize - size
                             in -- Replace block with smaller block
                                let swapped =
                                      s
                                        Vector.// [ (firstOpenBlockIndex, s Vector.! fileIndex),
                                                    (fileIndex, FreeSpace size)
                                                  ]
                                 in -- !z = traceShowId $ map toId $ Vector.toList swapped
                                    -- Insert diff block after fileIndex
                                    insertAt (firstOpenBlockIndex + 1) (FreeSpace diff) swapped

insertAt :: Int -> a -> Vector.Vector a -> Vector.Vector a
insertAt idx x vec
  | idx < 0 || idx > Vector.length vec = vec -- Handle invalid index
  | otherwise = (Vector.take idx vec) Vector.++ Vector.singleton x Vector.++ (Vector.drop idx vec)

toId :: Cell -> String
toId cell =
  case cell of
    File (FileDetails {size, id}) -> concat $ replicate (fromInteger size) $ show id
    FreeSpace n -> replicate (fromInteger n) '.'

-- 00...111...2...333.44.5555.6666.777.888899
-- 0099.111...2...333.44.5555.6666.777.8888..
-- 0099.1117772...333.44.5555.6666.....8888..
-- 0099.111777244.333....5555.6666.....8888..
-- 00992111777.44.333....5555.6666.....8888..

swapElements :: Int -> Int -> Vector.Vector a -> Vector.Vector a
swapElements i j vec =
  vec Vector.// [(i, vec Vector.! j), (j, vec Vector.! i)]

run :: IO ()
run = do
  content <- readFile "./app/Y2024/9.txt"
  -- let content = "2333133121414131402"

  -- Parse the line, with each index holding one item.
  let cells = map parseLine $ lines content

  -- Replicate to take up the right amount of space for each
  let !replicated :: [Vector.Vector Cell] =
        map Vector.fromList cells
  let !z = map toId $ Vector.toList $ head replicated
  let !rearranged :: [Vector.Vector Cell] = map move replicated
  let !replicated :: [Vector.Vector Cell] =
        map
          ( \a ->
              let
               in Vector.fromList . concatMap replicateCell $ a
          )
          rearranged
  let !d =
        map
          ( sum
              . zipWith
                ( \pos a ->
                    case a of
                      FreeSpace _ -> 0
                      File (FileDetails {id}) -> id * pos
                )
                [0 ..]
              . Vector.toList
          )
          replicated
  pPrint $ sum d
  return ()

-- 254068097059 too low