module Y2023.D3a (main) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Set as Set (Set, fromList, toList)
import Text.Pretty.Simple (pPrint)

data Span
  = Blank
  | Gear
  | Symbol Char
  | Number Int
  deriving (Show)

spanNumber :: Span -> Maybe Int
spanNumber (Number n) = Just n
spanNumber _ = Nothing

data Coordinate
  = Coordinate Int Int
  deriving (Show, Eq, Ord)

incrementX :: Int -> Coordinate -> Coordinate
incrementX n (Coordinate y x) = Coordinate y (x + n)

incrementY :: Int -> Coordinate -> Coordinate
incrementY n (Coordinate y x) = Coordinate (y + n) x

spanSize :: Span -> Int
spanSize Gear = 1
spanSize Blank = 1
spanSize (Symbol _) = 1
spanSize (Number n) = length (show n :: String)

standardize :: [Span] -> [Span]
standardize =
  concatMap (\x -> replicate (spanSize x) x)

parseLine :: String -> [Span]
parseLine [] = []
parseLine ('.' : xs) = Blank : parseLine xs
parseLine ('*' : xs) = Gear : parseLine xs
parseLine (c : rest)
  | isDigit c =
      let (digits, remaining) = span isDigit (c : rest)
       in Number (read digits) : parseLine remaining
  | otherwise = Symbol c : parseLine rest

surroundingCoords :: Coordinate -> [Coordinate]
surroundingCoords (Coordinate y x) =
  [ Coordinate (y - 1) (x - 1), -- top left
    Coordinate (y - 1) x, -- top
    Coordinate (y - 1) (x + 1), -- top right
    Coordinate y (x - 1), -- left
    Coordinate y (x + 1), -- right
    Coordinate (y + 1) (x - 1), -- bottom left
    Coordinate (y + 1) x, -- bottom
    Coordinate (y + 1) (x + 1) -- bottom right
  ]

valueAtCoordinate :: [[Span]] -> Coordinate -> Maybe Span
valueAtCoordinate standardized (Coordinate y x) =
  if y < 0 || x < 0 || y >= length standardized || x >= length (head standardized)
    then Nothing
    else Just $ standardized !! y !! x

surroundingNumbers :: Coordinate -> [[Span]] -> Set Int
surroundingNumbers coord standardized =
  let coords' = surroundingCoords coord
      surroundingNumbers' =
        mapMaybe
          ( \a ->
              (valueAtCoordinate standardized a :: Maybe Span) >>= spanNumber
          )
          coords'
   in fromList surroundingNumbers'

data State
  = State
  { count :: Int,
    coordinate :: Coordinate
  }

incrementCount :: Int -> State -> State
incrementCount n state' = state' {count = count state' + n}

main :: IO ()
main = do
  content <- readFile "./2023/3.txt"
  let actual :: [[Span]] = map parseLine . lines $ content
  let standardized :: [[Span]] = map standardize actual

  let state :: State =
        foldl
          ( \acc' a ->
              ( foldl
                  ( \acc'' span' ->
                      ( case span' of
                          Gear ->
                            let surroundingNumbers' = surroundingNumbers (coordinate acc'') standardized
                             in case Set.toList surroundingNumbers' of
                                  [a', b] ->
                                    incrementCount (a' * b) acc''
                                  _ -> acc''
                          _ -> acc''
                      )
                        { coordinate = incrementX (spanSize span') $ coordinate acc''
                        }
                  )
                  acc'
                  a
              )
                { coordinate = incrementY 1 $ coordinate acc'
                }
          )
          State {count = 0, coordinate = Coordinate 0 0}
          actual
  _ <- pPrint $ count state
  return ()
