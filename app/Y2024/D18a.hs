module Y2024.D18a (run) where

import Coordinate
import Control.Exception (throw)
import Data.List (intersperse)
import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))
import Data.Set qualified as Set

data Line
  = Line
  {}
  deriving (Show)

data State
  = State
  {}
  deriving (Show, Eq, Ord)

parseLine :: String -> Line
parseLine line =
  let regex = [r|([0-9]+): *(.+)|] :: String
      (_, _, _, [testNumber, numbers]) = line =~ regex :: (String, String, String, [String])
  in Line
        {
        }

run :: IO ()
run = do
  content <- readFile "./app/Y2024/18.txt"
  let lines' :: [Line] = map parseLine . lines $ content
  pPrint lines'

