module Y2024.D13a (run) where

import Control.Exception (throw)
import Coordinate
import Data.Function (on)
import Data.List (group, intersperse, minimumBy)
import Data.List.Split (splitOn)
import Data.Maybe qualified
import Data.Set qualified as Set
import Debug.Trace (trace, traceShow, traceShowId)
import Helpers
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data State
  = State
  {}
  deriving (Show, Eq, Ord)

data Group = Group
  { buttonAXPlus :: Integer,
    buttonAYPlus :: Integer,
    buttonBXPlus :: Integer,
    buttonBYPlus :: Integer,
    prize :: Coordinate
  }
  deriving (Show)

aCost :: Integer
aCost = 3

bCost :: Integer
bCost = 1

pressesToCost :: (Integer, Integer) -> Integer
pressesToCost (aPresses, bPresses) =
  aPresses * aCost + bPresses * bCost

minCost :: Group -> Integer
minCost group@Group {buttonAXPlus, buttonAYPlus, buttonBXPlus, buttonBYPlus} =
  let a1 = buttonAXPlus
      b1 = buttonBXPlus
      c1 = x (prize group)
      a2 = buttonAYPlus
      b2 = buttonBYPlus
      c2 = y (prize group)
      det = a1 * b2 - a2 * b1
   in if det == 0
        then 0
        else
          let detA = c1 * b2 - c2 * b1
              detB = a1 * c2 - a2 * c1
           in if detA `mod` det /= 0 || detB `mod` det /= 0
                then 0
                else
                  let a = detA `div` det
                      b = detB `div` det
                   in pressesToCost (a, b)

parseLine :: String -> Group
parseLine line =
  let regex = [r|Button A: X\+([0-9]+), Y\+([0-9]+)|] :: String
      (_, _, _, matchA) = line =~ regex :: (String, String, String, [String])
      regex' = [r|Button B: X\+([0-9]+), Y\+([0-9]+)|] :: String
      (_, _, _, matchB) = line =~ regex' :: (String, String, String, [String])
      regex'' = [r|Prize: X=([0-9]+), Y=([0-9]+)|] :: String
      (_, _, _, matchC) = line =~ regex'' :: (String, String, String, [String])
   in case (matchA, matchB, matchC) of
        ([aXPlus, aYPlus], [bXPlus, bYPlus], [prizeX, prizeY]) ->
          Group
            { buttonAXPlus = read aXPlus,
              buttonAYPlus = read aYPlus,
              buttonBXPlus = read bXPlus,
              buttonBYPlus = read bYPlus,
              prize = Coordinate {x = read prizeX + 10000000000000, y = read prizeY + 10000000000000}
            }
        _ ->
          throw $ userError $ "parseLine: " ++ line

run :: IO ()
run = do
  content <- readFile "./app/Y2024/13.txt"
  let lines' = splitOn "\n\n" content
  pPrint lines'
  let parsed = map parseLine lines'
  pPrint $ sum $ map minCost parsed
