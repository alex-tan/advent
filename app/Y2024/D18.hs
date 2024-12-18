{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Y2024.D18 (run) where

import Control.Exception (throw)
import Coordinate hiding (inBounds)
import Data.Function (on)
import Data.List (intersperse, minimumBy, sortOn)
import Data.List.Split (split, splitOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
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
  runSetup example

runSetup :: Setup -> IO ()
runSetup setup@Setup {coordinateCount} = do
  content <- readFile $ file setup
  let coordinates :: [Coordinate] = map parseLine . lines $ content
  -- let turnToCorrupted :: TurnToCorrupted = foldl fall Map.empty (zip coordinates [0 ..])
  let start = coordinateYX 0 0
  let trails =
        Trails
          { incomplete =
              [ Trail
                  { path = Set.singleton start,
                    turn = 0
                  }
              ],
            complete = [],
            leastSteps = Nothing
          }
  let state =
        complete $
          iterateTrails
            ( InvariantState
                { corruptedCoordinates = Set.fromList $ take coordinateCount coordinates,
                  bounds = setup.bounds,
                  finalCoordinate = setup.finalCoordinate
                }
            )
            trails
  pPrint $ minimumBy (compare `on` turn) state

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

nextCoordinates :: InvariantState -> Maybe Int -> Trail -> [Coordinate]
nextCoordinates InvariantState {bounds, corruptedCoordinates, finalCoordinate} leastSteps Trail {path, turn} =
  let calculated =
        Set.elemAt path 0
          |> fromJust
          |> addDirections
          |> filter
            ( \coord ->
                inBounds bounds coord
                  && not (Set.member coord corruptedCoordinates)
                  && not (Set.member coord path)
            )
          |> sortOn (Coordinate.rawDistance finalCoordinate)
   in maybe
        calculated
        ( \steps ->
            if turn > steps
              then []
              else calculated
        )
        leastSteps

inBounds :: Bounds -> Coordinate -> Bool
inBounds Bounds {maxX, maxY} Coordinate {x, y} =
  x >= 0 && x <= maxX && y >= 0 && y <= maxY

data Trails = Trails
  { incomplete :: [Trail],
    complete :: [Trail],
    leastSteps :: Maybe Int
  }

data Trail = Trail
  { path :: Set.OSet Coordinate,
    turn :: Int
  }
  deriving (Show, Eq, Ord)

data TrailNextStep
  = Continue [Trail]
  | CompleteUnsuccessfully
  | CompleteSuccessfully Trail

iterateTrails :: InvariantState -> Trails -> Trails
iterateTrails invariantState trails =
  let new =
        foldl
          ( \acc incompleteTrail ->
              case iterateTrail invariantState (leastSteps acc) incompleteTrail of
                Continue trails ->
                  acc {incomplete = trails ++ incomplete acc}
                CompleteUnsuccessfully ->
                  acc
                CompleteSuccessfully trail ->
                  acc
                    { complete = trail : complete acc,
                      leastSteps =
                        case leastSteps acc of
                          Nothing -> Just $ turn trail
                          Just steps -> Just $ min steps $ turn trail
                    }
          )
          trails {incomplete = []}
          (incomplete trails)
   in if null (incomplete new)
        then new
        else iterateTrails invariantState new

iterateTrail :: InvariantState -> Maybe Int -> Trail -> TrailNextStep
iterateTrail invariantState@InvariantState {finalCoordinate} leastSteps trail =
  let nextCoords = nextCoordinates invariantState leastSteps trail
   in case nextCoords of
        [] -> CompleteUnsuccessfully
        coords ->
          if finalCoordinate `elem` coords
            then
              CompleteSuccessfully $ addCoordinateToTrail trail finalCoordinate
            else
              coords
                |> map (addCoordinateToTrail trail)
                |> Continue

addCoordinateToTrail :: Trail -> Coordinate -> Trail
addCoordinateToTrail trail@Trail {path} coord =
  trail
    { path = (|<) coord path,
      turn = turn trail + 1
    }