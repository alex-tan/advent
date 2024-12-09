module Y2023.D4 (main) where

import Data.Set as Set (Set, fromList, intersection)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Line
  = Line
  { card :: Integer,
    winningNumbers :: [Integer],
    numbersHad :: [Integer]
  }
  deriving (Show)

overlap :: Line -> Set Integer
overlap line =
  Set.intersection (fromList $ winningNumbers line) (fromList $ numbersHad line)

score :: Line -> Integer
score line =
  let num = length $ overlap line
   in case num of
        0 -> 0
        1 -> 1
        _ -> 2 ^ (num - 1)

parseLine :: String -> Line
parseLine line =
  let regex = [r|Card *([0-9]+) *: *(.+) *\| *(.+)|] :: String
      (_, _, _, [card, winningNumbers, numbersHad]) = line =~ regex :: (String, String, String, [String])
   in Line
        { card = read card,
          winningNumbers = map read $ words winningNumbers,
          numbersHad = map read $ words numbersHad
        }

main :: IO ()
main = do
  content <- readFile "./2023/4.txt"
  let actual :: [Line] = map parseLine . lines $ content

  let state =
        foldl
          ( \acc' a ->
              acc' + score a
          )
          0
          actual
  pPrint state
