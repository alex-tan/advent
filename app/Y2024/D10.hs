{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Y2024.D10 (run) where

import Coordinate
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Direction4
import Helpers
import Text.Pretty.Simple (pPrint)

run :: IO ()
run = do
  content <- readFile "./app/Y2024/10.txt"
  let lines' :: [[Int]] = map (map (read . List.singleton)) $ lines content
  let map' = mapFromCells lines'
  let trailHeads :: [[(Coordinate.Coordinate, Int)]] =
        map'
          |> coordinateToCell
          |> Map.filter (== 0)
          |> Map.keys
          |> map (\c -> [(c, 0)])
  let trails :: [[(Coordinate, Int)]] = findTrails map' (FindTrails {incomplete = trailHeads, complete = []})
  pPrint $ length $ Set.fromList $ map (\a -> (fst $ head a, fst $ last a)) trails

findTrails :: Map Int -> FindTrails -> [[(Coordinate, Int)]]
findTrails map' findTrails' =
  if null $ incomplete findTrails'
    then
      filter ((==) 10 . length) $ map Prelude.reverse $ complete findTrails'
    else
      let processed =
            foldl
              ( \state@FindTrails {complete} partialTrail ->
                  let (coord, i) = head partialTrail
                      next = nextStep map' (i + 1) coord
                   in if Set.size next == 0
                        then
                          -- Add trail to complete when there are no next steps.
                          state
                            { complete = partialTrail : complete
                            }
                        else
                          state
                            { incomplete =
                                -- For each next step, add the next step to the partial trail.
                                next
                                  |> Set.toList
                                  |> map
                                    ( \(coord', int') ->
                                        let newTrail = (coord', int') : partialTrail
                                         in newTrail
                                    )
                                  -- Keep the other incompletes.
                                  ++ incomplete state
                            }
              )
              ( FindTrails
                  { complete = [],
                    incomplete = []
                  }
              )
              (incomplete findTrails')
       in findTrails map' processed

data FindTrails = FindTrails
  { complete :: [[(Coordinate, Int)]],
    incomplete :: [[(Coordinate, Int)]]
  }

nextStep :: Map Int -> Int -> Coordinate -> Set.Set (Coordinate, Int)
nextStep map' i coord =
  if i == 10
    then
      Set.empty
    else
      allDirections
        |> map (addDirection coord)
        |> mapMaybe
          ( \c ->
              let val = mapGetCoordinate map' c
               in if val == Just i
                    then
                      fmap (\v -> (c, v)) val
                    else
                      Nothing
          )
        |> Set.fromList
