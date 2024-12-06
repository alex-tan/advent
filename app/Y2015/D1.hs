module Y2015.D1 (run) where

import Text.Pretty.Simple (pPrint)

run :: IO ()
run = do
  content <- readFile "./app/Y2015/1.txt"
  let result = compute $ parseLine $ head $ lines content
  pPrint result

data Direction = Up | Down

compute :: [Direction] -> Integer
compute =
  foldl
    ( \floor d ->
        case d of
          Up ->
            floor + 1
          Down ->
            floor - 1
    )
    0

parseChar :: Char -> Direction
parseChar '(' = Up
parseChar ')' = Down
parseChar _ = error ""

parseLine :: String -> [Direction]
parseLine =
  map parseChar