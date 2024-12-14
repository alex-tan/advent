module Y2024.D14 (run) where

import Control.Exception (throw)
import Coordinate hiding (maxX, maxY)
import Data.List (group, groupBy, intersperse, sort)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShowId)
import GHC.List (iterate')
import Helpers
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Line
  = Line
  { coordinate :: Coordinate,
    velocity :: Coordinate
  }
  deriving (Show)

data State
  = State
  {}
  deriving (Show, Eq, Ord)

parseCell :: String -> Line
parseCell line =
  -- p=81,102 v=-72,40
  let regex = [r|p=([0-9]+),([0-9]+) v=([-0-9]+),([-0-9]+)|] :: String
      (_, _, _, sub) = line =~ regex :: (String, String, String, [String])
   in case sub of
        [x, y, vX, yX] ->
          Line
            { coordinate =
                Coordinate
                  { x = read x,
                    y = read y
                  },
              velocity =
                Coordinate
                  { x = read vX,
                    y = read yX
                  }
            }
        _ -> throw $ userError $ "parseCell: invalid input " ++ line

run :: IO ()
run = do
  content <- readFile "./app/Y2024/14.txt"
  let lines' :: [Line] = map parseCell $ lines content
  -- let lines' =
  --       [ Line
  --           { coordinate = Coordinate {x = 2, y = 4},
  --             velocity = Coordinate {x = 2, y = -3}
  --           }
  --       ]
  let state = iterate' next lines' !! 100 -- 5 6
  let quadrants =
        state
          |> mapMaybe quadrant
          |> sort
          |> traceShowId
          |> group
          |> traceShowId
          |> map length
          |> traceShowId
          |> product
  pPrint quadrants

next :: [Line] -> [Line]
next =
  map nextLine

countX = 101

maxX :: Integer
maxX = countX - 1

countY = 103

maxY :: Integer
maxY = countY - 1

nextLine :: Line -> Line
nextLine line@Line {coordinate, velocity} =
  let naiveCoord@Coordinate {x = newX, y = newY} = addCoordinate coordinate velocity
   in if newX > maxX || newY > maxY || newX < 0 || newY < 0
        then
          let newX'
                | newX < 0 = maxX + newX + 1
                | newX > maxX = newX - countX
                | otherwise = newX

              newY'
                | newY < 0 = maxY + newY + 1
                | newY > maxY = newY - countY
                | otherwise = newY
           in line
                { coordinate = Coordinate {x = newX', y = newY'}
                }
        else
          line
            { coordinate = naiveCoord
            }

quadrant :: Line -> Maybe Integer
quadrant Line {coordinate}
  | x == midX = Nothing
  | y == midY = Nothing
  | x < midX && y < midY = Just 1
  | x < midX && y > midY = Just 2
  | x > midX && y > midY = Just 3
  | x > midX && y < midY = Just 4
  where
    Coordinate {x, y} = coordinate

midX :: Integer
midX = maxX `div` 2

midY :: Integer
midY = maxY `div` 2
