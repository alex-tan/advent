module Direction8 where

import Coordinate

data Direction8
  = North
  | NorthEast
  | NorthWest
  | South
  | SouthEast
  | SouthWest
  | East
  | West
  deriving (Show, Eq, Ord, Bounded, Enum)

allDirections :: [Direction8]
allDirections = [minBound .. maxBound]

directionToAdjustment :: Direction8 -> Coordinate
directionToAdjustment North = Coordinate 0 (-1)
directionToAdjustment South = Coordinate 0 1
directionToAdjustment East = Coordinate 0 1
directionToAdjustment West = Coordinate 0 (-1)
directionToAdjustment NorthEast = Coordinate 1 (-1)
directionToAdjustment NorthWest = Coordinate (-1) (-1)
directionToAdjustment SouthEast = Coordinate 1 1
directionToAdjustment SouthWest = Coordinate (-1) 1