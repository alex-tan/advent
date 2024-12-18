module Direction4 where

import Coordinate
import Data.Hashable

data Direction4
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord, Bounded, Enum)

instance Hashable Direction4 where
  hashWithSalt salt North = hashWithSalt salt (0 :: Int)
  hashWithSalt salt East = hashWithSalt salt (1 :: Int)
  hashWithSalt salt South = hashWithSalt salt (2 :: Int)
  hashWithSalt salt West = hashWithSalt salt (3 :: Int)

minTurns :: Direction4 -> Direction4 -> Integer
minTurns d1 d2
  | d1 == d2 = 0
  | otherwise =
      case (d1, d2) of
        (North, South) -> 2
        (South, North) -> 2
        (East, West) -> 2
        (West, East) -> 2
        _ -> 1

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

turnRight :: Direction4 -> Direction4
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Direction4 -> Direction4
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South

addDirection :: Coordinate -> Direction4 -> Coordinate
addDirection coord dir = addCoordinate coord (directionToAdjustment dir)

relativeDirection :: Coordinate -> Coordinate -> Direction4
relativeDirection Coordinate {x = x1, y = y1} Coordinate {x = x2, y = y2}
  | y2 > y1 = South
  | y2 < y1 = North
  | x2 > x1 = East
  | x2 < x1 = West
  | otherwise = error "Not a relative direction"

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
