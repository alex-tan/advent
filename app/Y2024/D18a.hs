{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Y2024.D18a (run) where

import Control.Exception (throw)
import Coordinate hiding (inBounds)
import Data.Function (on)
import Data.Graph.AStar (aStar)
import Data.HashSet qualified as HashSet
import Data.List (find, intersperse, minimumBy, sortOn)
import Data.List.Split (split, splitOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isNothing)
import Data.Set.Ordered ((|<))
import Data.Set.Ordered qualified as Set
import Debug.Trace (trace, traceShowId)
import Direction4 (addDirections)
import Helpers ((|>))
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

type TurnToCorrupted = Map.Map Integer (Set.OSet Coordinate)

parseLine :: String -> Coordinate
parseLine line =
  case splitOn "," line of
    [x, y] ->
      Coordinate
        { x = read x,
          y = read y
        }
    _ -> throw $ userError "Invalid line"

-- fall :: TurnToCorrupted -> (Coordinate, Integer) -> TurnToCorrupted
-- fall state (coord, i) =
--   let prev = Map.findWithDefault Set.empty (i - 1) state
--    in Map.insert i (Set.insert coord prev) state

run :: IO ()
run = do
  let example =
        Setup
          { file = "./app/Y2024/18.example.txt",
            bounds = Bounds {maxX = 6, maxY = 6},
            finalCoordinate = coordinateYX 6 6,
            coordinateCount = 12
          }
  let actual =
        Setup
          { file = "./app/Y2024/18.txt",
            bounds = Bounds {maxX = 70, maxY = 70},
            finalCoordinate = coordinateYX 70 70,
            coordinateCount = 1024
          }
  runSetup actual

runSetup :: Setup -> IO ()
runSetup setup = do
  content <- readFile $ file setup
  let coordinates :: [Coordinate] = map parseLine . lines $ content
  let z =
        [1025 .. length coordinates - 1]
          |> find
            ( \count ->
                let invariant =
                      InvariantState
                        { corruptedCoordinates = Set.fromList $ take count coordinates,
                          bounds = setup.bounds,
                          finalCoordinate = setup.finalCoordinate
                        }
                 in isNothing $
                      aStar
                        (HashSet.fromList . nextCoordinates_ invariant)
                        rawDistance
                        (rawDistance setup.finalCoordinate)
                        (== setup.finalCoordinate)
                        (coordinateYX 0 0)
            )
  pPrint z

data Setup = Setup
  { file :: String,
    bounds :: Bounds,
    finalCoordinate :: Coordinate,
    coordinateCount :: Int
  }
  deriving (Show, Eq, Ord)

data InvariantState = InvariantState
  { corruptedCoordinates :: Set.OSet Coordinate,
    bounds :: Bounds,
    finalCoordinate :: Coordinate
  }
  deriving (Show, Eq, Ord)

data Bounds = Bounds
  { maxY :: Integer,
    maxX :: Integer
  }
  deriving (Show, Eq, Ord)

nextCoordinates_ :: InvariantState -> Coordinate -> [Coordinate]
nextCoordinates_ InvariantState {bounds, corruptedCoordinates, finalCoordinate} coord =
  let calculated =
        coord
          |> addDirections
          |> filter
            ( \coord ->
                inBounds bounds coord
                  && not (Set.member coord corruptedCoordinates)
            )
          |> sortOn (Coordinate.rawDistance finalCoordinate)
   in calculated

inBounds :: Bounds -> Coordinate -> Bool
inBounds Bounds {maxX, maxY} Coordinate {x, y} =
  x >= 0 && x <= maxX && y >= 0 && y <= maxY

-- 62,54 wrong