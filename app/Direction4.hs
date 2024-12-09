module Direction4 where

import Coordinate

data Direction4
  = North
  | South
  | East
  | West
  deriving (Show, Eq, Ord, Bounded, Enum)

reverse :: Direction4 -> Direction4
reverse North = South
reverse South = North
reverse East = West
reverse West = East

allDirections :: [Direction4]
allDirections = [minBound .. maxBound]

directionToAdjustment :: Direction4 -> Coordinate
directionToAdjustment North = Coordinate {x = 0, y = -1}
directionToAdjustment South = Coordinate {x = 0, y = 1}
directionToAdjustment East = Coordinate {x = 1, y = 0}
directionToAdjustment West = Coordinate {x = -1, y = 0}
