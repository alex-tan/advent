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

addDirection :: Coordinate -> Direction4 -> Coordinate
addDirection coord dir = addCoordinate coord (directionToAdjustment dir)

addDirections :: Coordinate -> [Coordinate]
addDirections coord =
  map (addDirection coord) allDirections

coordAdjacency :: Coordinate -> Coordinate -> Maybe Direction4
coordAdjacency Coordinate {x = x1, y = y1} Coordinate {x = x2, y = y2} =
  let xOff = (x1 - x2)
      yOff = (y1 - y2)
   in case (xOff, yOff) of
        (1, 0) -> Just East
        (-1, 0) -> Just West
        (0, 1) -> Just South
        (0, -1) -> Just North
        _ -> Nothing
