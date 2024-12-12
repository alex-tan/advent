module Region where

import Coordinate
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Debug.Trace (traceShowId)
import Direction4 (addDirections)
import Helpers
import Test.HUnit

data Region = Region
  { c :: Char,
    coords :: Set.Set Coordinate
  }
  deriving (Show, Eq, Ord)

tests :: Test
tests =
  let -- x
      region0 = Region.init 'a' (Coordinate 0 0)
      -- x x
      region1 = insert (Coordinate {y = 0, x = 1}) region0
      -- x x
      -- x
      region2 = insert (Coordinate {y = 1, x = 0}) region1
      -- x x x
      -- x   x
      -- x x x
      region3 =
        region2
          |> insert (Coordinate {y = 0, x = 2})
          |> insert (Coordinate {y = 1, x = 2})
          |> insert (Coordinate {y = 2, x = 0})
          |> insert (Coordinate {y = 2, x = 1})
          |> insert (Coordinate {y = 2, x = 2})
   in TestList
        [ TestCase $ assertEqual "area" 1 (area region0),
          TestCase $ assertEqual "perimeter" 4 (perimeter region0),
          TestCase $ assertEqual "price" 4 (price region0),
          TestCase $ assertEqual "area" 2 (area region1),
          TestCase $ assertEqual "perimeter" 6 (perimeter region1),
          TestCase $ assertEqual "price" 12 (price region1),
          TestCase $ assertEqual "region2 area" 3 (area region2),
          TestCase $ assertEqual "region2 perimeter" 8 (perimeter region2),
          TestCase $ assertEqual "region3 perimeter" 16 (perimeter region3)
          -- TestList $
          --   map
          --     ( \region ->
          --         TestCase $ assertEqual "price" 9 (price region)
          --     )
          --     [region0, region1, region2]
        ]

insert :: Coordinate -> Region -> Region
insert c r =
  r {coords = Set.insert c (coords r)}

fromList :: Char -> [Coordinate] -> Region
fromList a b = Region a $ Set.fromList b

area :: Region -> Int
area = Set.size . coords

perimeter :: Region -> Int
perimeter Region {coords} =
  coords
    |> Set.toList
    |> map
      ( \a ->
          addDirections a
            |> filter (`Set.notMember` coords)
            |> List.sort
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

regionsShouldBeMerged :: Region -> Region -> Bool
regionsShouldBeMerged Region {coords = a, c = char1} Region {coords = b, c = char2}
  | char1 /= char2 = False
  | otherwise =
      any
        ( \c ->
            any (coordAdjacentTo c) b
        )
        a

mergeRegions :: Region -> Region -> Region
mergeRegions a b =
  a
    { coords = Set.union (coords a) (coords b)
    }

combineRegions :: [Region] -> [Region]
combineRegions regions =
  let regions' :: [Region] = combineRegions' regions
      compare' = traceShowId . sum . map (Set.size . coords)
   in if compare' regions == compare' regions'
        then regions'
        else
          -- Continue merging.
          combineRegions regions'

combineRegions' :: [Region] -> [Region]
combineRegions' regions =
  concat $
    Map.elems $
      -- For each region.
      foldl
        ( \consumed region@Region {c} ->
            let regions' :: [Region] =
                  case Map.lookup c consumed :: Maybe [Region] of
                    -- If already grouped this character.
                    Just existing ->
                      -- If any regions are adjacent merge into existing ones.
                      if any (regionsShouldBeMerged region) existing
                        then
                          existing
                            |> map (\r -> if regionsShouldBeMerged r region then mergeRegions r region else r)
                        else
                          -- Otherwise add as a distinct region.
                          region : existing
                    Nothing ->
                      [region]
             in Map.insert c regions' consumed
        )
        (Map.empty :: Map.Map Char [Region])
        regions
