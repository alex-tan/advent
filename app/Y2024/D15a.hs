module Y2024.D15 (run) where

import Control.Exception (throw)
import Control.Monad (foldM)
import Coordinate
import Data.List (find, findIndex, intersperse)
import Data.List.Split (splitOn)
import Data.Map qualified
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Direction4
import Helpers ((|>))
import Test.HUnit
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Cell
  = Robot
  | Wall
  | Box
  | Blank
  deriving (Show, Eq, Ord)

parseCell :: Char -> Cell
parseCell '#' = Wall
parseCell 'O' = Box
parseCell '.' = Blank
parseCell '@' = Robot
parseCell _ = throw $ userError "invalid cell"

toChar :: Cell -> [Char]
toChar Wall = '#'
toChar Box = 'O'
toChar Blank = '.'
toChar Robot = '@'

parseDirection :: Char -> Direction4
parseDirection '^' = North
parseDirection 'v' = South
parseDirection '<' = West
parseDirection '>' = East
parseDirection _ = throw $ userError "invalid direction"

run :: IO ()
run = do
  content <- readFile "./app/Y2024/15.txt"
  let (a, b) = case splitOn "\n\n" content of
        [a, b] -> (a, b)
        _ -> throw $ userError "invalid input"

  let map' :: [[Cell]] = map (map parseCell) $ lines a
  let directions :: [Direction4] = concatMap (map parseDirection) $ lines b
  final <-
    foldM
      ( \map'' (dir, i) -> do
          let a' = next map'' dir
          writeFile ("./out/output_" ++ show i ++ ".txt") $ unlines $ map (map toChar) $ mapToCells a'
          return a'
      )
      (cellsToCoordMap map')
      (zip directions [1 ..])
  let boxLocs' = boxLocs final
  let gpsCoords = sum $ map gpsCoord boxLocs'
  pPrint map'
  pPrint directions
  pPrint gpsCoords
  runTestTT tests
  return ()

next :: Map.Map Coordinate Cell -> Direction4 -> Map.Map Coordinate Cell
next coordinateToCell' direction =
  let robotLoc = findRobot coordinateToCell'
   in case robotLoc of
        Just loc -> shift direction loc coordinateToCell'
        Nothing -> throw $ userError "robot not found"

shift :: Direction4 -> Coordinate -> Map.Map Coordinate Cell -> Map.Map Coordinate Cell
shift direction coord0 original =
  let coord1 = addDirection coord0 direction
      cell0 = fromJust $ Data.Map.lookup coord0 original
      cell1 = fromJust $ Data.Map.lookup coord1 original
   in case (cell0, cell1) of
        -- Walls can't move.
        (Wall, _) -> original
        -- Can't move if a wall is blocking.
        (_, Wall) -> original
        -- If a box is in the way, check if it can be moved.
        (_, Box) ->
          let attemptToMoveBox = shift direction coord1 original
           in -- Try to move box
              if attemptToMoveBox == original
                then original
                else
                  attemptToMoveBox
                    |> Data.Map.insert coord0 Blank
                    |> Data.Map.insert coord1 cell0
        _ ->
          original
            |> Data.Map.insert coord0 Blank
            |> Data.Map.insert coord1 cell0

gpsCoord :: Coordinate -> Integer
gpsCoord Coordinate {x, y} = 100 * y + x

boxLocs :: Map.Map Coordinate Cell -> [Coordinate]
boxLocs cells =
  cells
    |> Data.Map.toList
    |> filter (\(_, cell) -> cell == Box)
    |> map fst

findRobot :: Map.Map Coordinate Cell -> Maybe Coordinate
findRobot cells =
  cells
    |> Data.Map.toList
    |> find (\(_, cell) -> cell == Robot)
    |> fmap fst

-- 1478519 wrong
-- 731195 wrong
-- 718246 wrong

tests :: Test
tests =
  TestList
    [ TestLabel "shift" $
        let testShift desc before after coord dir =
              TestCase $
                assertEqual
                  desc
                  after
                  (mapToCells $ shift dir coord $ cellsToCoordMap before)
         in TestList
              [ let cells' = [[Wall, Robot]]
                 in testShift
                      "robot blocked by wall"
                      cells'
                      cells'
                      (coordinateYX 0 1)
                      West,
                let cells' = [[Wall, Box, Robot]]
                 in testShift
                      "robot blocked by box blocked by wall"
                      cells'
                      cells'
                      (coordinateYX 0 2)
                      West,
                testShift
                  "box to blank"
                  [[Wall, Blank, Box, Robot]]
                  [[Wall, Box, Robot, Blank]]
                  (coordinateYX 0 3)
                  West,
                testShift
                  "box and robot shift left"
                  [[Wall, Box, Blank, Box, Robot]]
                  [[Wall, Box, Box, Robot, Blank]]
                  (coordinateYX 0 4)
                  West,
                testShift
                  "box and robot shift left 2"
                  [[Wall, Blank, Box, Blank, Box, Robot]]
                  [[Wall, Blank, Box, Box, Robot, Blank]]
                  (coordinateYX 0 5)
                  West
              ]
    ]