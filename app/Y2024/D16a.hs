module Y2024.D16a (run) where

import Control.Exception (assert, throw)
import Coordinate (rawDistance)
import Coordinate qualified
import Data.Function (on)
import Data.Graph.AStar (aStar)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
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

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '#' = Wall
parseTile 'S' = StartTile
parseTile 'E' = EndTile
parseTile _ = throw $ userError "Invalid tile"

parseMap :: String -> MapDetails
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
   in MapDetails
        { map' = map',
          end = end,
          start = start
        }

run :: IO ()
run = do
  -- content <- readFile "./app/Y2024/16.txt"
  content <- readFile "./app/Y2024/16.txt"
  let mapDetails = parseMap content
  let path :: [(Direction4, Coordinate.Coordinate)] =
        fromJust $
          aStar
            (HashSet.fromList . nextSteps mapDetails)
            ( \(dirA, a) (dirB, b) ->
                let minTurns' = traceShowId $ minTurns dirA dirB
                 in Coordinate.rawDistance a b + (minTurns' * 1000)
            )
            (\(_, coord) -> Coordinate.rawDistance mapDetails.end coord)
            (\(_, coord) -> coord == mapDetails.end)
            (East, mapDetails.start)

  let state =
        foldl
          ( \(dir, coord, score) (nextDir, nextCoord) ->
              let minTurns' = minTurns dir nextDir
               in ( nextDir,
                    nextCoord,
                    score + 1 + (minTurns' * 1000)
                  )
          )
          (East, mapDetails.start, 0)
          path
  pPrint $ length path
  pPrint state

-- 169464 too high
-- 125464 too high

nextSteps :: MapDetails -> (Direction4, Coordinate.Coordinate) -> [(Direction4, Coordinate.Coordinate)]
nextSteps MapDetails {map'} (dir, coord) =
  allDirections
    |> mapMaybe
      ( \newDir ->
          let newCoord = addDirection coord newDir
              relDir = relativeDirection coord newCoord
           in case Map.lookup newCoord $ Coordinate.coordinateToCell map' of
                Just Empty ->
                  Just (relDir, newCoord)
                Just EndTile ->
                  Just (relDir, newCoord)
                _ ->
                  Nothing
      )

data MapDetails = MapDetails
  { map' :: Coordinate.Map Tile,
    end :: Coordinate.Coordinate,
    start :: Coordinate.Coordinate
  }
  deriving (Show)
