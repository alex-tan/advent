module Coordinate where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Helpers

data Coordinate = Coordinate
  { x :: Integer,
    y :: Integer
  }
  deriving (Show, Ord, Eq)

data LinesData
  = LinesData
  { minX :: Integer,
    maxX :: Integer,
    minY :: Integer,
    maxY :: Integer
  }
  deriving (Show)

data Direction
  = North
  | NorthEast
  | NorthWest
  | South
  | SouthEast
  | SouthWest
  | East
  | West
  deriving (Show, Eq, Ord, Bounded, Enum)

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

directionToAdjustment :: Direction -> Coordinate
directionToAdjustment North = Coordinate 0 (-1)
directionToAdjustment South = Coordinate 0 1
directionToAdjustment East = Coordinate 0 1
directionToAdjustment West = Coordinate 0 (-1)
directionToAdjustment NorthEast = Coordinate 1 (-1)
directionToAdjustment NorthWest = Coordinate (-1) (-1)
directionToAdjustment SouthEast = Coordinate 1 1
directionToAdjustment SouthWest = Coordinate (-1) 1

multCoordinate :: Coordinate -> Coordinate -> Coordinate
multCoordinate (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 * x2) (y1 * y2)

addCoordinate :: Coordinate -> Coordinate -> Coordinate
addCoordinate (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)

distance :: Coordinate -> Coordinate -> Coordinate
distance (Coordinate x1 y1) (Coordinate x2 y2) =
  Coordinate (abs (x1 - x2)) (abs (y1 - y2))

offset :: Coordinate -> Coordinate -> Coordinate
offset (Coordinate x1 y1) (Coordinate x2 y2) =
  Coordinate (x1 - x2) (y1 - y2)

negateCoordinate :: Coordinate -> Coordinate
negateCoordinate (Coordinate x y) = Coordinate (-x) (-y)

inBounds :: LinesData -> Coordinate -> Bool
inBounds linesData' coord' =
  x coord' >= minX linesData'
    && x coord' <= maxX linesData'
    && y coord' >= minY linesData'
    && y coord' <= maxY linesData'

linesData :: [Coordinate] -> LinesData
linesData positions =
  let ys = map y positions
      xs = map x positions
   in LinesData
        { minX = minimum xs,
          maxX = maximum xs,
          minY = minimum ys,
          maxY = maximum ys
        }

deltas :: LinesData -> Set.Set Coordinate
deltas linesData' =
  let minY' = minY linesData'
      maxY' = maxY linesData'
      minX' = minX linesData'
      maxX' = maxX linesData'
   in Set.fromList
        [ Coordinate x y
          | x <- [minX' .. maxX'],
            y <- [minY' .. maxY']
        ]

drawCoordinates :: [Coordinate] -> [String]
drawCoordinates positions =
  let linesData' = linesData positions
      coordinates :: Set.Set Coordinate =
        positions
          |> Set.fromList
   in if abs (minY linesData' - maxY linesData') > 1000
        then []
        else
          [minY linesData' .. maxY linesData']
            |> map
              ( \y ->
                  [minX linesData' .. maxX linesData']
                    |> map
                      ( \x ->
                          if Set.member (Coordinate x y) coordinates
                            then
                              '#'
                            else
                              ' '
                      )
              )

cellsToCoordMap :: [[a]] -> Map.Map Coordinate a
cellsToCoordMap cells =
  foldl
    ( \acc (y, row) ->
        foldl
          ( \acc' (x, cell) ->
              Map.insert (Coordinate x y) cell acc'
          )
          acc
          (zip [0 ..] row)
    )
    Map.empty
    (zip [0 ..] cells)