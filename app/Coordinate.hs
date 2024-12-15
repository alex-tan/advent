module Coordinate where

import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Helpers

data Coordinate = Coordinate
  { y :: Integer,
    x :: Integer
  }
  deriving (Show, Ord, Eq)

empty :: Coordinate
empty = Coordinate 0 0

data LinesData
  = LinesData
  { minX :: Integer,
    maxX :: Integer,
    minY :: Integer,
    maxY :: Integer
  }
  deriving (Show)

coordAdjacentTo :: Coordinate -> Coordinate -> Bool
coordAdjacentTo Coordinate {x = x1, y = y1} Coordinate {x = x2, y = y2} =
  let xOff = abs (x1 - x2)
      yOff = abs (y1 - y2)
   in xOff <= 1 && yOff <= 1 && xOff /= yOff

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

drawCoordinates' :: Integer -> Integer -> [Coordinate] -> [String]
drawCoordinates' maxY maxX positions =
  let coordinates :: Set.Set Coordinate =
        positions
          |> Set.fromList
   in [0 .. maxY]
        |> map
          ( \y ->
              [0 .. maxX]
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
              Map.insert (Coordinate {y = y, x = x}) cell acc'
          )
          acc
          (zip [0 ..] row)
    )
    Map.empty
    (zip [0 ..] cells)

data Map a = Map
  { cells :: [[a]],
    coordinateToCell :: Map.Map Coordinate a,
    bounds :: LinesData
  }
  deriving (Show)

instance (Eq (a)) => Eq (Map a) where
  (==) a b = cells a == cells b

coordinateYX :: Integer -> Integer -> Coordinate
coordinateYX y x = Coordinate {y = y, x = x}

coordinateXY :: Integer -> Integer -> Coordinate
coordinateXY x y = Coordinate {y = y, x = x}

mapToCells :: Map.Map Coordinate a -> [[a]]
mapToCells coordMap =
  let linesData' = linesData $ Map.keys coordMap
   in [minY linesData' .. maxY linesData']
        |> map
          ( \y ->
              [minX linesData' .. maxX linesData']
                |> map
                  ( \x ->
                      fromJust $ Map.lookup (Coordinate {x = x, y = y}) coordMap
                  )
          )

mapFromCells :: [[a]] -> Map a
mapFromCells cells' =
  Map
    { cells = cells',
      coordinateToCell = cellsToCoordMap cells',
      bounds = linesData $ Map.keys $ cellsToCoordMap cells'
    }

inMapBounds :: Map a -> Coordinate -> Bool
inMapBounds map' = inBounds (bounds map')

mapGetCoordinate :: Map a -> Coordinate -> Maybe a
mapGetCoordinate map' coord' = Map.lookup coord' (coordinateToCell map')
