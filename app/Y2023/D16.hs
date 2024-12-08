module Y2023.D16 (run) where

import Control.Exception (throw)
import Coordinate
import Data.List (intersperse)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (trace, traceShowId)
import Direction4
import Helpers
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data State
  = State
  { beams :: [(Coordinate, Direction4)],
    energizedTiles :: Set.Set Coordinate
  }
  deriving (Show, Eq, Ord)

next :: Map.Map Coordinate Cell -> LinesData -> State -> State
next map' linesData' State {beams, energizedTiles} =
  let newBeams =
        beams
          |> concatMap
            ( \(coord, direction) ->
                let cell = Map.findWithDefault Blank coord map'
                    action' = action cell direction
                    adjCoord dir = (addCoordinate coord $ directionToAdjustment dir, dir)
                 in case action' of
                      Continue ->
                        [adjCoord direction]
                      Reflect direction' ->
                        [adjCoord direction']
                      Split direction1 direction2 ->
                        [ adjCoord direction1,
                          adjCoord direction2
                        ]
            )
          |> filter (\(coord, _) -> inBounds linesData' coord)
      newEnergizedTiles = Set.union energizedTiles $ Set.fromList $ map fst newBeams
   in State
        { beams = newBeams,
          energizedTiles = newEnergizedTiles
        }

data Cell
  = Blank
  | MirrorRL -- /
  | MirrorLR -- \
  | VerticalSplitter
  | HorizontalSplitter
  deriving (Show, Eq, Ord)

data Action
  = Continue
  | Reflect Direction4
  | Split Direction4 Direction4

action :: Cell -> Direction4 -> Action
action Blank _ = Continue
action MirrorRL North = Reflect East
action MirrorRL South = Reflect West
action MirrorRL East = Reflect North
action MirrorRL West = Reflect South
action MirrorLR North = Reflect West
action MirrorLR South = Reflect East
action MirrorLR East = Reflect South
action MirrorLR West = Reflect North
action VerticalSplitter East = Split North South
action VerticalSplitter West = Split North South
action VerticalSplitter _ = Continue
action HorizontalSplitter North = Split East West
action HorizontalSplitter South = Split East West
action HorizontalSplitter _ = Continue

parseChar :: Char -> Cell
parseChar '.' = Blank
parseChar '/' = MirrorRL
parseChar '\\' = MirrorLR
parseChar '|' = VerticalSplitter
parseChar '-' = HorizontalSplitter
parseChar _ = throw $ userError "Invalid character"

parseLine :: String -> [Cell]
parseLine =
  map parseChar

run :: IO ()
run = do
  content <- readFile "./app/Y2023/D16.txt"
  let lines' :: [[Cell]] = map parseLine . lines $ content
  let map' = cellsToCoordMap lines'
  let linesData' = traceShowId $ linesData $ Map.keys map'
  let initialState =
        State
          { beams = [(Coordinate {y = 1, x = 0}, South)],
            energizedTiles = Set.fromList [Coordinate 0 0, Coordinate {x = 0, y = 1}]
          }
  let a =
        foldl
          (\state _ -> next map' linesData' state)
          initialState
          [0 .. 1000]
          |> energizedTiles
          |> Set.size
  pPrint a
