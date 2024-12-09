module Y2018.D10 (run) where

import Control.Monad (foldM)
import Data.Set qualified as Set
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Line
  = Line
  { position :: Coordinate,
    velocity :: Coordinate
  }
  deriving (Show)

(|>) :: a -> (a -> b) -> b
x |> f = f x

data LinesData
  = LinesData
  { minX :: Integer,
    maxX :: Integer,
    minY :: Integer,
    maxY :: Integer
  }
  deriving (Show)

linesData :: [Line] -> LinesData
linesData lines' =
  let positions =
        lines'
          |> map position
      ys = map y positions
      xs = map x positions
   in LinesData
        { minX = minimum xs,
          maxX = maximum xs,
          minY = minimum ys,
          maxY = maximum ys
        }

drawLines :: [Line] -> [String]
drawLines lines' =
  let positions =
        lines'
          |> map position
      linesData' = linesData lines'
      coordinates :: Set.Set Coordinate =
        positions
          |> Set.fromList
   in if abs (minY linesData' - maxY linesData') > 1000
        then []
        else
          [minY linesData' .. maxY linesData']
            |> map
              ( \y ->
                  [minX linesData' .. maxX linesData']
                    |> map
                      ( \x ->
                          if Set.member (Coordinate x y) coordinates
                            then
                              '#'
                            else
                              ' '
                      )
              )

adjustLine :: Line -> Line
adjustLine line =
  line
    { position = addCoordinate (position line) (velocity line)
    }

data Coordinate = Coordinate
  { x :: Integer,
    y :: Integer
  }
  deriving (Show, Ord, Eq)

addCoordinate :: Coordinate -> Coordinate -> Coordinate
addCoordinate (Coordinate x1 y1) (Coordinate x2 y2) = Coordinate (x1 + x2) (y1 + y2)

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
              let drawn = drawLines new
              if null drawn
                then return new
                else do
                  writeFile ("./out/output_" ++ show i ++ ".txt") $ unlines drawn
                  return new
          )
          lines'
    return ()
