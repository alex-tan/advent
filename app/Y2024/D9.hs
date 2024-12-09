{-# LANGUAGE QuasiQuotes #-}

module Y2024.D9 (run) where

import Control.Exception (throw)
import Control.Monad (forM)
import Coordinate
import Data.List (intersperse)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as Vector
import Debug.Trace (trace, traceShow, traceShowId)
import GHC.Stats (GCDetails (gcdetails_par_balanced_copied_bytes))
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
  = File ID Integer
  | FreeSpace Integer
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
                    (n + 1, File n a : list)
                  else
                    ( n,
                      if a == 0 then list else FreeSpace a : list
                    )
        )
        (0 :: Integer, [])
        (zip [0 ..] line)

expand :: Cell -> [Cell]
expand cell =
  case cell of
    File i n -> replicate (fromIntegral n) cell
    FreeSpace n -> replicate (fromIntegral n) cell

toStr :: Cell -> Integer
toStr cell =
  case cell of
    File i n -> n
    FreeSpace n -> n

toId :: Cell -> String
toId cell =
  case cell of
    File i n -> concat $ replicate (fromInteger n) $ show i
    FreeSpace n -> replicate (fromInteger n) '.'

replicateCell :: Cell -> [Cell]
replicateCell cell =
  case cell of
    File i n -> replicate (fromInteger n) cell
    FreeSpace n -> replicate (fromInteger n) cell

isFile :: Cell -> Bool
isFile (File _ _) = True
isFile _ = False

isFreeSpace :: Cell -> Bool
isFreeSpace (FreeSpace _) = True
isFreeSpace _ = False

move :: Vector.Vector Cell -> Vector.Vector Cell
move s =
  let firstOpenBlockIndex :: Int = head $ filter (\i -> isFreeSpace $ s Vector.! i) [0 .. length s - 1]
      allNumerics :: [Int] =
        filter
          (\i -> isFile $ s Vector.! i)
          [firstOpenBlockIndex .. length s - 1]
      lastNumericIndex :: Int = last allNumerics
   in if null allNumerics
        then
          s
        else
          let swapped = swapElements firstOpenBlockIndex lastNumericIndex s
           in move swapped

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
        map
          ( \a ->
              let
               in Vector.fromList . concatMap replicateCell $ a
          )
          cells
  print "B"
  let !rearranged :: [Vector.Vector Cell] = map move replicated
  let !d =
        map
          ( sum
              . zipWith
                ( \pos a ->
                    case a of
                      FreeSpace _ -> 0
                      File a _ -> a * pos
                )
                [0 ..]
              . Vector.toList
          )
          rearranged
  pPrint $ sum d
  return ()

-- 85734297767 too low
-- real: 00...111...2...333.44.5555.6666.777.888899
-- mine: 00...111...2...333.44.5555.6666.777.888899