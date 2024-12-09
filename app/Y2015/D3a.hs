{-# LANGUAGE QuasiQuotes #-}

module Y2015.D3a (run) where

import Control.Exception (throw)
import Coordinate qualified
import Data.List (intersperse)
import Data.Set qualified as Set
import Debug.Trace (trace)
import Direction4 qualified
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

parseChar :: Char -> Direction4.Direction4
parseChar '>' = Direction4.East
parseChar '<' = Direction4.West
parseChar '^' = Direction4.North
parseChar 'v' = Direction4.South
parseChar _ = throw $ userError "Invalid character"

data State
  = State
  { coordinateA :: Coordinate.Coordinate,
    coordinateB :: Coordinate.Coordinate,
    visited :: Set.Set Coordinate.Coordinate
  }
  deriving (Show, Eq, Ord)

run :: IO ()
run = do
  content <- readFile "./app/Y2015/3.txt"
  let directions :: [Direction4.Direction4] = map parseChar $ head $ lines content
  let state =
        foldl
          ( \state'@State {coordinateA, coordinateB, visited} (direction, i) ->
              if even i
                then
                  let a' = Coordinate.addCoordinate (Direction4.directionToAdjustment direction) coordinateA
                   in state'
                        { coordinateA = a',
                          visited = Set.insert a' visited
                        }
                else
                  let b' = Coordinate.addCoordinate (Direction4.directionToAdjustment direction) coordinateB
                   in state'
                        { coordinateB = b',
                          visited = Set.insert b' visited
                        }
          )
          State
            { coordinateA = Coordinate.empty,
              coordinateB = Coordinate.empty,
              visited = Set.singleton Coordinate.empty
            }
          (zip directions [1 ..])
  pPrint $ Set.size $ visited state

-- 4161 too high