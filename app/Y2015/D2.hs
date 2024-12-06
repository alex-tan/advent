module Y2015.D2 (run) where

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

sideAreas :: Box -> [Integer]
sideAreas box =
  [ length' box * width box,
    width box * height box,
    height box * length' box
  ]

slack :: Box -> Integer
slack =
  minimum . sideAreas

totalArea :: Box -> Integer
totalArea box =
  slack box + sum (map (* 2) $ sideAreas box)

compute :: [Box] -> Integer
compute =
  sum . map totalArea

parseLine :: String -> Box
parseLine line =
  let vals :: [Integer] = map read $ splitOn "x" line
   in Box
        { length' = vals !! 0,
          width = vals !! 1,
          height = vals !! 2
        }
