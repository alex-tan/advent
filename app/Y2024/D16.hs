module Y2024.D16 (run) where

import Control.Exception (throw)
import Coordinate qualified
import Data.Function (on)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShowId)
import Direction4
import Direction4 qualified as Coordinate
import GHC.Generics qualified as Coordinate
import Helpers
import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA (Extract (empty), (=~))

data Tile
  = EndTile
  | StartTile
  | Wall
  | Empty
  deriving (Show, Eq, Ord)

data State = State
  { visitedCoordinates :: Set.Set (Coordinate.Coordinate, Direction4),
    visitedCoordinatesOrdered :: [Coordinate.Coordinate],
    score :: Integer,
    coordinate :: Coordinate.Coordinate,
    direction :: Direction4,
    lastChokepoint :: Maybe (Coordinate.Coordinate, Direction4)
  }
  deriving (Show, Eq, Ord)

maybeAdjustDirection :: Direction4 -> State -> State
maybeAdjustDirection newDir state =
  if newDir == direction state
    then state
    else
      state
        { direction = newDir,
          score = score state + (1000 * minTurns (direction state) newDir)
        }

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '#' = Wall
parseTile 'S' = StartTile
parseTile 'E' = EndTile
parseTile _ = throw $ userError "Invalid tile"

parseMap :: String -> (MapDetails, State)
parseMap content =
  let lines' = map (map parseTile) $ lines content
      map' = Coordinate.mapFromCells lines'
      coordinateToCell' = Coordinate.coordinateToCell map'
      start :: Coordinate.Coordinate =
        coordinateToCell'
          |> Map.filter (== StartTile)
          |> Map.keys
          |> head

      end :: Coordinate.Coordinate =
        coordinateToCell'
          |> Map.filter (== EndTile)
          |> Map.keys
          |> head
   in ( MapDetails
          { map' = map',
            end = end,
            start = start,
            chokepoints = Set.empty,
            bestScore = 100000000000000000000
          },
        State
          { visitedCoordinates = Set.singleton (start, East),
            visitedCoordinatesOrdered = [start],
            score = 0,
            coordinate = start,
            direction = East,
            lastChokepoint = Nothing
          }
      )

run :: IO ()
run = do
  content <- readFile "./app/Y2024/16.txt"
  let (mapDetails, firstState) = parseMap content
  -- pPrint mapDetails
  let trails :: [State] =
        findTrails
          ( mapDetails,
            ( FindTrails
                { incomplete = [firstState],
                  complete = []
                }
            )
          )
  pPrint $ List.minimumBy (compare `on` score) trails

data MapDetails = MapDetails
  { map' :: Coordinate.Map Tile,
    end :: Coordinate.Coordinate,
    start :: Coordinate.Coordinate,
    chokepoints :: Set.Set (Coordinate.Coordinate, Direction4),
    bestScore :: Integer
  }
  deriving (Show)

findTrails :: (MapDetails, FindTrails) -> [State]
findTrails (map', findTrails')
  | null $ incomplete findTrails' =
      findTrails'
        |> complete
  -- \|> filter (\s -> coordinate s == end map')
  | otherwise =
      let processed =
            foldl
              ( \(map'', state@FindTrails {incomplete, complete}) partialState ->
                  if coordinate partialState == end map''
                    then
                      -- Update the score if the end was reached.
                      ( map''
                          { bestScore = min (score partialState) (bestScore map'')
                          },
                        state
                          { complete = traceShowId partialState : complete
                          }
                      )
                    else
                      let nextSets :: Set.Set State = nextSteps map'' partialState
                       in -- When there are no next steps
                          if Set.size nextSets == 0
                            then
                              ( map'',
                                state
                              )
                            else
                              ( map'',
                                state
                                  { incomplete =
                                      -- For each next step, add the next step to the partial trail.
                                      nextSets
                                        |> Set.toList
                                        -- Keep the other incompletes.
                                        ++ incomplete
                                  }
                              )
              )
              ( map',
                FindTrails
                  { complete = [],
                    incomplete = []
                  }
              )
              (incomplete findTrails')
       in findTrails processed

data FindTrails = FindTrails
  { complete :: [State],
    incomplete :: [State]
  }

nextSteps :: MapDetails -> State -> Set.Set State
nextSteps details@MapDetails {end} state
  -- If the end has already been reached stop
  | end == coordinate state = Set.empty
  -- If the score is higher than the best score, stop
  | score state > bestScore details = Set.empty
  | otherwise =
      allDirections
        |> map
          (\dir -> (dir, addDirection (coordinate state) dir))
        |> mapMaybe (maybeNextState details state)
        |> Set.fromList

maybeNextState ::
  MapDetails ->
  State ->
  (Direction4, Coordinate.Coordinate) ->
  Maybe State
maybeNextState MapDetails {map', chokepoints} state@State {visitedCoordinates} (dir, newCoordinate)
  -- If it's a chokepoint stop
  | Set.member (newCoordinate, dir) chokepoints = Nothing
  -- If it's been visited before in this trail, stop.
  | Set.member (newCoordinate, dir) visitedCoordinates = Nothing
  | otherwise =
      case Map.lookup newCoordinate coordinateToCell of
        -- If it's a wall stop.
        Just Wall -> Nothing
        -- If it's not in the map, stop.
        Nothing -> Nothing
        _ ->
          addCoordinate dir newCoordinate
            |> maybeAdjustDirection dir
            |> Just
  where
    coordinateToCell :: Map.Map Coordinate.Coordinate Tile = Coordinate.coordinateToCell map'
    addCoordinate :: Direction4 -> Coordinate.Coordinate -> State
    addCoordinate direction coord =
      state
        { visitedCoordinates = Set.insert (coord, direction) visitedCoordinates,
          visitedCoordinatesOrdered = coord : visitedCoordinatesOrdered state,
          coordinate = coord,
          score = score state + 1,
          direction = direction
        }

-- if Set.size (nextSteps details state) == 1
--     then a {lastChokepoint = Just (coord, direction)}
--     else a

-- 846596 too high
-- tests :: Test
-- tests =
--   TestList
--     [ TestLabel "shift" $
--         TestList
--           [ -- #E###
--             -- #.###
--             -- #S..#
--             -- #####
--             let cells' =
--                   [ [Wall, EndTile, Wall, Wall, Wall],
--                     [Wall, Empty, Wall, Wall, Wall],
--                     [Wall, StartTile, Empty, Empty, Wall],
--                     [Wall, Wall, Wall, Wall, Wall]
--                   ]
--              in testShift
--                   "robot blocked by wall"
--                   cells'
--                   Nothing
--                   (coordinateYX 0 2)
--                   West
--           ]
--     ]