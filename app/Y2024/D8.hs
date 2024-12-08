{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Y2024.D8 (run) where

import Coordinate
import Data.Map qualified as Map
import Data.Set qualified as Set
import Helpers
import Text.Pretty.Simple (pPrint)

data Cell
  = Blank
  | Antenna Char
  deriving (Show, Eq, Ord)

parseChar :: Char -> Cell
parseChar '.' = Blank
parseChar c = Antenna c

parseLine :: String -> [Cell]
parseLine =
  map parseChar

run :: IO ()
run = do
  content <- readFile "./app/Y2024/8.txt"
  let cells :: [[Cell]] = map parseLine $ lines content

  -- Iterate through each cell and create a coordinate map
  let coords :: Map.Map Coordinate Cell =
        foldl
          ( \acc (y, row) ->
              foldl
                ( \acc' (x, cell) ->
                    Map.insert (Coordinate x y) cell acc'
                )
                acc
                (zip [0 ..] row)
          )
          Map.empty
          (zip [0 ..] cells)

  let coordsList :: [Coordinate] = Map.keys coords
  let linesData' :: LinesData = linesData coordsList
  let antennasByChar :: Map.Map Char (Set.Set Coordinate) =
        foldl
          ( \acc (coord, cell) ->
              case cell of
                Antenna c ->
                  Map.insertWith Set.union c (Set.fromList [coord]) acc
                _ -> acc
          )
          Map.empty
          (Map.toList coords)

  -- for each cell identify lines
  -- Set (Set Coordinate)
  let antinodes :: Set.Set Coordinate =
        Set.fromList $
          concatMap
            ( \coords ->
                let -- all permutations of coords
                    pairs = [(a, b) | a <- Set.toList coords, b <- Set.toList coords, a /= b]
                 in pairs
                      |> concatMap
                        ( \(a, b) ->
                            let offset' = offset a b
                                negated = negateCoordinate offset'
                             in [addCoordinate offset' a, addCoordinate negated b]
                                  |> filter (inBounds linesData')
                        )
            )
            antennasByChar

  pPrint $ length antinodes