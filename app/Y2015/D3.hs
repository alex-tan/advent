{-# LANGUAGE QuasiQuotes #-}

module Y2015.D3 (run) where

import Control.Exception (throw)
import Coordinate
import Data.Set qualified as Set
import Direction4
import Text.Pretty.Simple (pPrint)

parseChar :: Char -> Direction4
parseChar '>' = East
parseChar '<' = West
parseChar '^' = North
parseChar 'v' = South
parseChar _ = throw $ userError "Invalid character"

data State
  = State
  { coordinate :: Coordinate,
    visited :: Set.Set Coordinate
  }
  deriving (Show, Eq, Ord)

run :: IO ()
run = do
  content <- readFile "./app/Y2015/3.txt"
  let directions :: [Direction4] = map parseChar $ head $ lines content
  let state =
        foldl
          ( \State {coordinate, visited} direction ->
              let newCoordinate = addCoordinate (directionToAdjustment direction) coordinate
               in State
                    { coordinate = newCoordinate,
                      visited = Set.insert newCoordinate visited
                    }
          )
          State {coordinate = Coordinate {x = 0, y = 0}, visited = Set.singleton $ Coordinate {x = 0, y = 0}}
          directions
  pPrint $ Set.size $ visited state
