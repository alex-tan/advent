module Y2024.D12 (run) where

import Control.Exception (throw)
import Coordinate
import Data.List (intersperse, sort)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Traversable (for)
import Debug.Trace (trace, traceShowId)
import Direction4 (addDirection, allDirections)
import Helpers
import Region qualified
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

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
run = do
  content <- readFile "./app/Y2024/12.txt"
  let lines' :: [[Char]] = lines $ content
  let map' :: Map Char = mapFromCells lines'
  let a =
        foldl
          ( \state@State {regions} (line, y) ->
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
            ( \r ->
                r
                  { Region.coords =
                      Set.fromList $
                        sort $
                          Set.toList $
                            Region.coords r
                  }
            )
          $ regions a

  let letter = 'Z'
  _ <-
    for
      regions'
      ( \region@Region.Region {c} -> do
          if c == letter
            then do
              pPrint $ c
              pPrint $ Region.area region
              pPrint $ Region.perimeter region
              pPrint $ Region.price region
              print ""
            else
              return ()
      )

  pPrint $ length $ filter (\Region.Region {c} -> c == letter) regions'

  pPrint $ sum $ map Region.price $ regions'

-- 3504556 Wrong
-- 1566650 too high
-- 1537894 too high
