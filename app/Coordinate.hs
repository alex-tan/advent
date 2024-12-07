module Coordinate where

data Coordinate = Coordinate
  { x :: Integer,
    y :: Integer
  }
  deriving (Show, Ord, Eq)