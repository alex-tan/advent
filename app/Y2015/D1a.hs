module Y2015.D1a (run) where

import Text.Pretty.Simple (pPrint)

run :: IO ()
run = do
  content <- readFile "./app/Y2015/1.txt"
  let result = compute $ parseLine $ head $ lines content
  pPrint result

data Direction = Up | Down

data State = State
  { currentPos :: Integer,
    pos :: Maybe Integer,
    currentFloor :: Integer
  }

move :: Integer -> Direction -> Integer
move floor' d =
  case d of
    Up ->
      floor' + 1
    Down ->
      floor' - 1

compute :: [Direction] -> Maybe Integer
compute =
  pos
    . foldl
      ( \acc d ->
          case pos acc of
            Just _ ->
              acc
            Nothing ->
              let floor' = move (currentFloor acc) d
               in State
                    { currentPos = currentPos acc + 1,
                      pos = if floor' == -1 then Just $ currentPos acc else Nothing,
                      currentFloor = floor'
                    }
      )
      ( State
          { pos = Nothing,
            currentPos = 1,
            currentFloor = 0
          }
      )

parseChar :: Char -> Direction
parseChar '(' = Up
parseChar ')' = Down
parseChar _ = error ""

parseLine :: String -> [Direction]
parseLine =
  map parseChar