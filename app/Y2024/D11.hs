module Y2024.D11 (run) where

import Control.Exception (throw)
import Coordinate
import Data.List (intersperse)
import Data.Set qualified as Set
import Debug.Trace (trace, traceShowId)
import Helpers
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

-- If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
-- If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
-- If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
changeStone :: Int -> [Int]
changeStone 0 = [1]
changeStone n =
  let str = show n
   in if even $ length str
        then
          let (left, right) = splitAt (length str `div` 2) str
           in [ read $ removeLeading0s left,
                read $ removeLeading0s right
              ]
        else
          [n * 2024]

-- Haskell remove leading 0s
removeLeading0s :: String -> String
removeLeading0s s =
  let a = dropWhile (== '0') s
   in if null a then "0" else a

run :: IO ()
run = do
  -- let content = "125 17"
  content <- readFile "./app/Y2024/11.txt"
  let ints :: [Int] = map read $ words content
  let z =
        foldl
          ( \acc _ ->
              concatMap changeStone acc
          )
          ints
          [0 .. 24]
  pPrint $ length $ z
