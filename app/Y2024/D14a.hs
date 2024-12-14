module Y2024.D14a (run) where

import Control.Concurrent (threadDelay)
import Control.Exception (throw)
import Control.Monad (forM)
import Coordinate hiding (maxX, maxY)
import Data.List (group, groupBy, intersperse, sort)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShowId)
import Direction4 (addDirections)
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
  forM
    (zip [0 ..] $ iterate' next lines')
    ( \(i, lines'') -> do
        let cells = map coordinate lines''
        print i
        if hasManyDiagonal cells > 200
          then
            let drawn = drawCoordinates' maxY maxX cells
             in writeFile ("./out/output_" ++ show i ++ ".txt") $ unlines drawn
          else
            pure ()
    )
    >> pure ()

hasManyDiagonal :: [Coordinate] -> Int
hasManyDiagonal cells =
  let set = Set.fromList cells
   in cells
        |> filter
          ( \cell ->
              addDirections cell
                |> any (\c -> Set.member c set)
          )
        |> length

isDiagonal :: Coordinate -> Coordinate -> Bool
isDiagonal a b =
  x a == x b + 1 && y a == y b + 1

next :: [Line] -> [Line]
next =
  map nextLine

countX :: Integer
countX = 101

maxX :: Integer
maxX = countX - 1

countY :: Integer
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

-- 1042 too low
-- 3706 too low