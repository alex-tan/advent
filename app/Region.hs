module Region where

import Coordinate
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Debug.Trace (traceShowId)
import Direction4 (Direction4 (..), addDirection, addDirections, allDirections, coordAdjacency)
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
          TestCase $ assertEqual "region3 perimeter" 16 (perimeter region3),
          TestCase $ assertEqual "shareBorder" True (shareBorder (North, Coordinate 0 0) (North, Coordinate 0 1)),
          TestCase $ assertEqual "shareBorder 2" True (shareBorder (South, Coordinate 0 0) (South, Coordinate 0 1)),
          TestCase $ assertEqual "shareBorder 3" False (shareBorder (West, Coordinate 0 0) (West, Coordinate 0 1)),
          TestCase $ assertEqual "shareBorder 4" False (shareBorder (East, Coordinate 0 0) (East, Coordinate 0 1))
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

priceA :: Region -> Int
priceA a = sides a * area a

sides :: Region -> Int
sides Region {coords} =
  let start :: [(Direction4, Coordinate)] =
        coords
          |> Set.toList
          |> concatMap
            ( \coord ->
                allDirections
                  |> Maybe.mapMaybe
                    ( \dir ->
                        let coord' = addDirection coord dir
                         in if Set.notMember coord' coords
                              then
                                Just (dir, coord')
                              else
                                Nothing
                    )
            )
   in start
        |> foldl
          ( \acc item ->
              if any (any (shareBorder item)) acc
                then
                  acc
                    |> map
                      ( \(group :: [(Direction4, Coordinate)]) ->
                          if any (shareBorder item) group
                            then
                              item : group
                            else
                              group
                      )
                else [item] : acc
          )
          ([] :: [[(Direction4, Coordinate)]])
        |> length

shareBorder :: (Direction4, Coordinate) -> (Direction4, Coordinate) -> Bool
shareBorder (dirA, coordA) (dirB, coordB)
  | dirA /= dirB = False
  | otherwise =
      case (coordAdjacency coordA coordB, dirA) of
        (Just West, North) -> True
        (Just West, South) -> True
        (Just East, South) -> True
        (Just East, North) -> True
        (Just North, East) -> True
        (Just North, West) -> True
        (Just South, West) -> True
        (Just South, East) -> True
        _ -> False

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
