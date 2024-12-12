module Region where

import Coordinate
import Data.Map qualified as Map
import Data.Set qualified as Set
import Direction4
import Helpers

data Region = Region
  { c :: Char,
    coords :: Set.Set Coordinate
  }
  deriving (Show, Eq, Ord)

area :: Region -> Int
area = Set.size . coords

perimeter :: Region -> Int
perimeter Region {coords} =
  coords
    |> Set.toList
    |> map
      ( \a ->
          allDirections
            |> map (addDirection a)
            |> filter (`Set.notMember` coords)
            |> length
      )
    |> sum

price :: Region -> Int
price a = area a * perimeter a

init :: Char -> Coordinate -> Region
init c coord = Region {c = c, coords = Set.singleton coord}

adjacentTo :: Region -> Coordinate -> Char -> Bool
adjacentTo region@Region {c} coord char =
  (c == char) && any (coordAdjacentTo coord) (coords region)

regionsAdjacent :: Region -> Region -> Bool
regionsAdjacent Region {coords = a, c = char1} Region {coords = b, c = char2} =
  if char1 /= char2
    then False
    else
      any
        ( \c ->
            any (coordAdjacentTo c) b
        )
        (a)

mergeRegions :: Region -> Region -> Region
mergeRegions a b =
  a
    { coords = Set.union (coords a) (coords b)
    }

combineRegions :: [Region] -> [Region]
combineRegions regions =
  let regions' = combineRegions' regions
      compare = sum . map (Set.size . coords)
   in if compare regions == compare regions'
        then regions
        else combineRegions regions'

combineRegions' :: [Region] -> [Region]
combineRegions' regions =
  concat $
    Map.elems $
      foldl
        ( \consumed region@Region {c} ->
            let regions' :: [Region] =
                  case Map.lookup c consumed :: Maybe [Region] of
                    Just existing ->
                      if any (regionsAdjacent region) existing
                        then
                          existing
                            |> map (\r -> if regionsAdjacent r region then mergeRegions r region else r)
                        else
                          region : existing
                    Nothing ->
                      [region]
             in Map.insert c regions' consumed
        )
        (Map.empty :: Map.Map Char [Region])
        regions
