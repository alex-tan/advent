module Y2024.D12a (run) where

import Coordinate
import Data.List (groupBy, sort)
import Data.Set qualified as Set
import Debug.Trace (traceShowId)
import Helpers
import Region qualified
import Test.HUnit (runTestTT)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ ()

data State
  = State
  { regions :: [Region.Region]
  }
  deriving (Show, Eq, Ord)

combineIntoRegion :: Region.Region -> (Coordinate, Char) -> (Region.Region, Bool)
combineIntoRegion region (coord, char) =
  if Region.adjacentTo region coord char
    then
      (region {Region.coords = Set.insert coord (Region.coords region)}, True)
    else
      (region, False)

next :: State -> Integer -> (Char, Integer) -> State
next state'@State {regions = regions'} y (char, x) =
  let coord = Coordinate {y = y, x = x}
      regions'' =
        regions'
          |> map
            ( \region ->
                combineIntoRegion region (coord, char)
            )
   in if any snd regions''
        then
          state' {regions = map fst regions''}
        else
          state' {regions = Region.init char coord : regions'}

run :: IO ()
run =
  do
    content <- readFile "./app/Y2024/12.txt"
    let lines' :: [[Char]] = lines content
    let map' :: Map Char = mapFromCells lines'
    let a =
          foldl
            ( \state (line, y) ->
                foldl
                  ( \state' (char, x) ->
                      next state' y (char, x)
                  )
                  state
                  (zip line [0 ..])
            )
            State {regions = []}
            (zip lines' [0 ..])

    let regions' =
          Region.combineRegions
            $ map
              ( \r' ->
                  r'
                    { Region.coords = Set.fromList $ sort $ Set.toList $ Region.coords r'
                    }
              )
            $ regions a

    let !regions'' =
          groupBy (\a b -> Region.c a == Region.c b) regions'
            |> map (\a -> (Region.c $ head a, length a))
            |> traceShowId

    _ <- runTestTT Region.tests
    let !regions'' =
          groupBy (\a b -> Region.c a == Region.c b) regions'
            |> map (\a -> (Region.c $ head a, length a))
            |> traceShowId

    pPrint $ sort $ filter ((== 'C') . Region.c) regions'

    pPrint $ sum $ map Region.priceA $ Set.toList $ Set.fromList $ regions'

-- 692 too low
