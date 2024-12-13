module Y2024.D13a (run) where

import Control.Exception (throw)
import Coordinate
import Data.Function (on)
import Data.HashMap (mapMaybe)
import Data.List (intersperse, minimumBy)
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

pressesToCoordinateAndCost :: Group -> (Integer, Integer) -> (Coordinate, Integer)
pressesToCoordinateAndCost Group {buttonAXPlus, buttonAYPlus, buttonBXPlus, buttonBYPlus} (aPresses, bPresses) =
  ( Coordinate
      { x = (aPresses * buttonAXPlus) + (bPresses * buttonBXPlus),
        y = (aPresses * buttonAYPlus) + (bPresses * buttonBYPlus)
      },
    aPresses * aCost + bPresses * bCost
  )

combos :: Group -> [(Integer, Integer)]
combos Group {buttonAXPlus, buttonAYPlus, buttonBXPlus, buttonBYPlus, prize} =
  let Coordinate {x = prizeX, y = prizeY} = prize
      aXMax = prizeX `div` buttonAXPlus
      bXMax = prizeX `div` buttonBXPlus
      aYMax = prizeY `div` buttonAYPlus
      bYMax = prizeY `div` buttonBYPlus
      aMax = min aXMax aYMax
      bMax = min bXMax bYMax
   in [(a, b) | a <- [0 .. aMax], b <- [0 .. bMax]]

minCost :: Group -> Integer
minCost group@Group {prize} =
  let aBCombos :: [(Integer, Integer)] = combos group
      !z = traceShowId group
      combos' =
        aBCombos
          |> Data.Maybe.mapMaybe
            ( \combo' ->
                let pair = pressesToCoordinateAndCost group combo'
                    (coord, cost) = pair
                 in if coord == prize then Just (combo', cost) else Nothing
            )
      combo =
        combos'
          |> minimumBy (compare `on` snd)
   in if null combos'
        then 0
        else
          snd combo

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
              prize = Coordinate {x = read prizeX, y = read prizeY}
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
