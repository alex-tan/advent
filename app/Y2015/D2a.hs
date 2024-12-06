module Y2015.D2a (run) where

import Data.List (sort)
import Data.List.Split (splitOn)
import Text.Pretty.Simple (pPrint)

run :: IO ()
run = do
  content <- readFile "./app/Y2015/2.txt"
  let boxes = map parseLine $ lines content
  let result = compute boxes
  pPrint result

data Box = Box
  { length' :: Integer,
    width :: Integer,
    height :: Integer
  }
  deriving (Show)

dimensons :: Box -> [Integer]
dimensons box =
  [ length' box,
    width box,
    height box
  ]

wrapping :: Box -> [Integer]
wrapping =
  map (* 2) . take 2 . sort . dimensons

bow :: Box -> Integer
bow box =
  length' box * width box * height box

totalRibbon :: Box -> Integer
totalRibbon box = sum $ bow box : wrapping box

compute :: [Box] -> Integer
compute =
  sum . map totalRibbon

parseLine :: String -> Box
parseLine line =
  let vals :: [Integer] = map read $ splitOn "x" line
   in Box
        { length' = vals !! 0,
          width = vals !! 1,
          height = vals !! 2
        }
