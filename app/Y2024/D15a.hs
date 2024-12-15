{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant curry" #-}
module Y2024.D15a (run) where

import Control.Exception (throw)
import Control.Monad (foldM)
import Coordinate
import Data.Function (on)
import Data.List (find, groupBy, minimumBy)
import Data.List.Split (splitOn)
import Data.Map qualified
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Direction4
import Helpers ((|>))
import Test.HUnit
import Text.Pretty.Simple (pPrint)

data Cell
  = Robot
  | Wall
  | Box Coordinate
  | Blank
  deriving (Show, Eq, Ord)

parseCell :: Coordinate -> Char -> [Cell]
parseCell _ '#' = [Wall, Wall]
parseCell coord 'O' = [Box coord, Box coord]
parseCell _ '.' = [Blank, Blank]
parseCell _ '@' = [Robot, Blank]
parseCell _ _ = throw $ userError "invalid cell"

parseDirection :: Char -> Direction4
parseDirection '^' = North
parseDirection 'v' = South
parseDirection '<' = West
parseDirection '>' = East
parseDirection _ = throw $ userError "invalid direction"

toChar :: Cell -> [Char]
toChar Wall = "#"
toChar (Box _) = "["
toChar Blank = "."
toChar Robot = "@"

run :: IO ()
run = do
  content <- readFile "./app/Y2024/15.txt"
  let (a, b) = case splitOn "\n\n" content of
        [a, b] -> (a, b)
        _ -> throw $ userError "invalid input"

  let map' :: [[Cell]] =
        zipWith
          ( curry
              ( \(line, y) ->
                  concatMap
                    ( \(char, x) ->
                        parseCell (coordinateYX y x) char
                    )
                    (zip line [0 ..])
              )
          )
          (lines a)
          [0 ..]
  let directions :: [Direction4] = concatMap (map parseDirection) $ lines b
  final <-
    foldM
      ( \map'' (dir, i) -> do
          let a' = next map'' dir
          writeFile ("./out/output_" ++ show i ++ ".txt") $ unlines $ map (concatMap toChar) $ mapToCells a'
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
        Just loc -> fromMaybe coordinateToCell' $ shift direction loc coordinateToCell'
        Nothing -> throw $ userError "robot not found"

shift :: Direction4 -> Coordinate -> Map.Map Coordinate Cell -> Maybe (Map.Map Coordinate Cell)
shift direction coord0 original =
  let coord1 = addDirection coord0 direction
      cell0 = fromJust $ Data.Map.lookup coord0 original
      cell1 = fromJust $ Data.Map.lookup coord1 original
   in case (cell0, cell1) of
        -- Walls can't move.
        (Wall, _) -> Nothing
        -- Can't move if a wall is blocking.
        (_, Wall) -> Nothing
        -- If a box is in the way, check if it can be moved.
        (_, Box _) ->
          let pairedBox =
                coord1
                  |> addDirections
                  |> find (\coord -> Map.lookup coord original == Just cell1)
           in let adjusted =
                    -- Try to move box
                    pairedBox
                      >>= (\p -> shift direction p original)
                      >>= shift direction coord1
               in adjusted
                    |> fmap (Data.Map.insert coord0 Blank)
                    |> fmap (Data.Map.insert coord1 cell0)
        _ ->
          original
            |> Data.Map.insert coord0 Blank
            |> Data.Map.insert coord1 cell0
            |> Just

gpsCoord :: Coordinate -> Integer
gpsCoord Coordinate {x, y} = 100 * y + x

boxLocs :: Map.Map Coordinate Cell -> [Coordinate]
boxLocs cells =
  cells
    |> Data.Map.toList
    |> filter (\(_, cell) -> isBox cell)
    |> groupBy (\a b -> snd a == snd b)
    |> map (minimumBy (compare `on` fst))
    |> map fst

isBox :: Cell -> Bool
isBox (Box _) = True
isBox _ = False

findRobot :: Map.Map Coordinate Cell -> Maybe Coordinate
findRobot cells =
  cells
    |> Data.Map.toList
    |> find (\(_, cell) -> cell == Robot)
    |> fmap fst

tests :: Test
tests =
  TestList
    [ TestLabel "shift" $
        let testShift desc before expected coord dir =
              TestCase $
                assertEqual
                  desc
                  expected
                  (fmap mapToCells $ shift dir coord $ cellsToCoordMap before)
            box1 = Box $ coordinateXY 0 0
            box2 = Box $ coordinateXY 1 0
            box3 = Box $ coordinateXY 2 0
         in TestList
              [ let cells' = [[Wall, Wall, Robot]]
                 in testShift
                      "robot blocked by wall"
                      cells'
                      Nothing
                      (coordinateYX 0 2)
                      West,
                let cells' = [[Wall, Wall, box1, box1, Robot]]
                 in testShift
                      "robot blocked by box blocked by wall"
                      cells'
                      Nothing
                      (coordinateYX 0 4)
                      West,
                testShift
                  "box to blank"
                  [[Wall, Blank, box1, box1, Robot]]
                  (Just [[Wall, box1, box1, Robot, Blank]])
                  (coordinateYX 0 4)
                  West,
                testShift
                  "box and robot shift left"
                  [[Wall, box1, box1, Blank, box2, box2, Robot]]
                  (Just [[Wall, box1, box1, box2, box2, Robot, Blank]])
                  (coordinateYX 0 6)
                  West,
                testShift
                  "box and robot shift left 2"
                  [[Wall, Blank, box1, box1, Blank, box2, box2, Robot]]
                  (Just [[Wall, Blank, box1, box1, box2, box2, Robot, Blank]])
                  (coordinateYX 0 7)
                  West,
                testShift
                  "push multiple boxes"
                  [ [Blank, Blank, Blank, Blank],
                    [box1, box1, box2, box2],
                    [Blank, box3, box3, Blank],
                    [Blank, Robot, Blank, Blank]
                  ]
                  ( Just
                      [ [box1, box1, box2, box2],
                        [Blank, box3, box3, Blank],
                        [Blank, Robot, Blank, Blank],
                        [Blank, Blank, Blank, Blank]
                      ]
                  )
                  (coordinateYX 3 1)
                  North
              ]
    ]