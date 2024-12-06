{-# LANGUAGE QuasiQuotes #-}

module Y2024.D6 (run) where

import Data.List (elemIndex)
import Data.Map as Map (Map, fromList, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set as Set (Set, fromList, insert, intersection, size, toList)
import Debug.Trace (trace, traceShow)
import GHC.Desugar ((>>>))
import System.Posix.Internals (statGetType)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Line
  = Guard
  | Obstruction
  | Blank
  deriving (Show, Eq, Ord)

parseLine :: String -> [Line]
parseLine =
  map parseChar

parseChar :: Char -> Line
parseChar '^' = Guard
parseChar '#' = Obstruction
parseChar '.' = Blank

data State
  = State
  { visited :: Set Coordinate,
    coordinate :: Coordinate,
    guardDirection :: Direction
  }
  deriving (Show, Eq, Ord)

data Coordinate
  = Coordinate Integer Integer
  deriving (Show, Eq, Ord)

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq, Ord)

directionToAdjustment :: Direction -> (Integer, Integer)
directionToAdjustment North = (-1, 0)
directionToAdjustment South = (1, 0)
directionToAdjustment East = (0, 1)
directionToAdjustment West = (0, -1)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

locationOfGuard :: [[Line]] -> Coordinate
locationOfGuard map' =
  let (y, x) =
        head $
          mapMaybe
            ( \(row, lines) ->
                case elemIndex Guard lines of
                  Just index -> Just (row, index)
                  Nothing -> Nothing
            )
            (zip [0 ..] map')
   in Coordinate (fromIntegral y) (fromIntegral x)

inBounds :: Coordinate -> [[Line]] -> Bool
inBounds (Coordinate y x) map' =
  x >= 0 && y >= 0 && y < fromIntegral (length map') && x < fromIntegral (length (head map'))

complete :: [[Line]] -> State -> State
complete map' state =
  let next' = next map' state
   in if next' == state then state else complete map' next'

next :: [[Line]] -> State -> State
next map' state =
  let direction' = directionToAdjustment $ guardDirection state
      nextCoordinate' = adjustCoordinate direction' (coordinate state)
      inBounds' = inBounds nextCoordinate' map'
   in if inBounds'
        then
          let blocked' = blocked nextCoordinate' map'
           in if blocked'
                then
                  turn state
                else
                  advance nextCoordinate' state
        else
          state

blocked :: Coordinate -> [[Line]] -> Bool
blocked (Coordinate y x) map' =
  case map' !! fromIntegral y !! fromIntegral x of
    Obstruction -> True
    _ -> False

advance :: Coordinate -> State -> State
advance coord state =
  state {coordinate = coord, visited = Set.insert coord $ visited state}

turn :: State -> State
turn state =
  let direction' = turnRight $ guardDirection state
   in state {guardDirection = direction'}

adjustCoordinate :: (Integer, Integer) -> Coordinate -> Coordinate
adjustCoordinate (y, x) (Coordinate y' x') = Coordinate (y' + y) (x' + x)

run :: IO ()
run = do
  content <- readFile "./app/Y2024/6.txt"
  let map' :: [[Line]] = map parseLine . lines $ content

  let loc' = locationOfGuard map'

  let state :: State =
        complete
          map'
          ( State
              { visited = Set.fromList [loc'],
                coordinate = locationOfGuard map',
                guardDirection = North
              }
          )
  _ <- pPrint $ coordinate state
  _ <- pPrint $ Set.size $ visited state
  return ()
