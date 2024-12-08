{-# LANGUAGE QuasiQuotes #-}

module Y2018.D10a (run) where

import Control.Monad (foldM)
import Coordinate
import Helpers
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Line
  = Line
  { position :: Coordinate,
    velocity :: Coordinate
  }
  deriving (Show)

adjustLine :: Line -> Line
adjustLine line =
  line
    { position = addCoordinate (position line) (velocity line)
    }

data State
  = State
  {}
  deriving (Show, Eq, Ord)

parseLine :: String -> Line
parseLine line =
  -- position=<-10651, -54021> velocity=< 1,  5>
  let regex = [r|position=< *([-0-9]+), *([-0-9]+)> +velocity=< *([-0-9]+), * ([-0-9]+)>|] :: String
      (_, _, _, [posX, posY, velX, velY]) = line =~ regex :: (String, String, String, [String])
   in Line
        { position = Coordinate (read posX) (read posY),
          velocity = Coordinate (read velX) (read velY)
        }

adjustLines :: [Line] -> [Line]
adjustLines =
  map adjustLine

run :: IO ()
run =
  do
    content <- readFile "./app/Y2018/10.txt"
    let lines' :: [Line] = map parseLine . lines $ content
    _ <-
      [0 .. 1000000]
        |> foldM
          ( \lines' i -> do
              let new = adjustLines lines'
              let drawn = drawCoordinates $ map position new
              if null drawn
                then return new
                else do
                  writeFile ("./out/output_" ++ show i ++ ".txt") $ unlines drawn
                  return new
          )
          lines'
    return ()
