module Y2024.D6a (run) where

import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import Data.Set as Set (Set, filter, fromList, insert, member)
import Data.Vector as Vector (Vector, fromList, length, toList, (!), (//))
import Text.Pretty.Simple (pPrint)

data Line
  = Guard
  | Obstruction
  | Blank
  deriving (Show, Eq, Ord)

isBlank :: Line -> Bool
isBlank Blank = True
isBlank _ = False

parseLine :: String -> Vector Line
parseLine =
  Vector.fromList . map parseChar

parseChar :: Char -> Line
parseChar '^' = Guard
parseChar '#' = Obstruction
parseChar '.' = Blank

data State
  = State
  { visited :: Set (Coordinate, Direction),
    coordinate :: Coordinate,
    guardDirection :: Direction
  }
  deriving (Show, Eq, Ord)

data Coordinate
  = Coordinate Integer Integer
  deriving (Show, Eq, Ord)

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq, Ord)

directionToAdjustment :: Direction -> (Integer, Integer)
directionToAdjustment North = (-1, 0)
directionToAdjustment South = (1, 0)
directionToAdjustment East = (0, 1)
directionToAdjustment West = (0, -1)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

locationOfGuard :: Map' -> Coordinate
locationOfGuard map' =
  let (y, x) =
        head $
          mapMaybe
            ( \(row, lines) ->
                case elemIndex Guard lines of
                  Just index -> Just (row, index)
                  Nothing -> Nothing
            )
            (zip [0 ..] $ map Vector.toList $ Vector.toList map')
   in Coordinate (fromIntegral y) (fromIntegral x)

atCoord :: Map' -> Coordinate -> Line
atCoord map' (Coordinate y x) =
  map' ! fromIntegral y ! fromIntegral x

inBounds :: Coordinate -> Map' -> Bool
inBounds (Coordinate y x) map' =
  x >= 0 && y >= 0 && y < fromIntegral (Vector.length map') && x < fromIntegral (Vector.length (map' ! 0))

complete :: Map' -> State -> Bool
complete map' state =
  let next' = next map' state
      direction' = directionToAdjustment $ guardDirection state
      nextCoordinate' = adjustCoordinate direction' (coordinate state)
      alreadyVisited = Set.member (nextCoordinate', guardDirection next') $ visited state
   in if alreadyVisited
        then
          True
        else
          if next' /= state
            then
              complete map' next'
            else
              False

next :: Map' -> State -> State
next map' state =
  let direction' = directionToAdjustment $ guardDirection state
      nextCoordinate' = adjustCoordinate direction' (coordinate state)
      inBounds' = inBounds nextCoordinate' map'
   in if inBounds'
        then
          let blocked' = blocked nextCoordinate' map'
           in if blocked'
                then
                  turn state
                else
                  advance nextCoordinate' state
        else
          state

blocked :: Coordinate -> Map' -> Bool
blocked (Coordinate y x) map' =
  case map' ! fromIntegral y ! fromIntegral x of
    Obstruction -> True
    _ -> False

advance :: Coordinate -> State -> State
advance coord state =
  state {coordinate = coord, visited = Set.insert (coord, guardDirection state) $ visited state}

turn :: State -> State
turn state =
  let direction' = turnRight $ guardDirection state
   in state {guardDirection = direction'}

adjustCoordinate :: (Integer, Integer) -> Coordinate -> Coordinate
adjustCoordinate (y, x) (Coordinate y' x') = Coordinate (y' + y) (x' + x)

updateCoord :: Coordinate -> Line -> Map' -> Map'
updateCoord (Coordinate y x) line map' =
  map'
    Vector.// [(fromIntegral y, (map' ! fromIntegral y) Vector.// [(fromIntegral x, line)])]

type Map' = Vector (Vector Line)

run :: IO ()
run = do
  content <- readFile "./app/Y2024/6.txt"
  let map' :: Map' = Vector.fromList $ map parseLine . lines $ content

  let loc' = locationOfGuard map'
  let initialState =
        State
          { visited = Set.fromList [(loc', North)],
            coordinate = locationOfGuard map',
            guardDirection = North
          }

  -- let state :: State = complete map' initialState
  -- Get all coords
  let allCoords = [Coordinate y x | y <- [0 .. fromIntegral (Vector.length map') - 1], x <- [0 .. fromIntegral (Vector.length (map' ! 0)) - 1]]
  let allBlankCoords = Set.fromList $ Prelude.filter (isBlank . atCoord map') allCoords
  let a =
        Set.filter
          ( \coord ->
              let map'' = updateCoord coord Obstruction map'
                  result = complete map'' initialState
               in result
          )
          allBlankCoords

  pPrint $ Prelude.length a
