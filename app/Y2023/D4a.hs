{-# LANGUAGE QuasiQuotes #-}

module Y2023.D4a (run) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set (Set, fromList, intersection)
import Debug.Trace (traceShow, traceShowId)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

type CardNumber = Integer

data Line
  = Line
  { cardNumber :: CardNumber,
    winningNumbers :: [Integer],
    numbersHad :: [Integer]
  }
  deriving (Show)

overlap :: Line -> Set Integer
overlap line =
  Set.intersection
    (Set.fromList $ winningNumbers line)
    (Set.fromList $ numbersHad line)

score :: Line -> Integer
score line =
  fromIntegral $ length $ overlap line

parseLine :: String -> Line
parseLine line =
  let regex = [r|Card *([0-9]+) *: *(.+) *\| *(.+)|] :: String
      (_, _, _, [card, winningNumbers, numbersHad]) = line =~ regex :: (String, String, String, [String])
   in Line
        { cardNumber = read card,
          winningNumbers = map read $ words winningNumbers,
          numbersHad = map read $ words numbersHad
        }

run :: IO ()
run = do
  content <- readFile "./app/Y2023/4.txt"
  let lines' :: [Line] = map parseLine . lines $ content
  let state =
        -- Card 1 has four matching numbers, so you win one copy each of the next four cards: cards 2, 3, 4, and 5.
        -- Your original card 2 has two matching numbers, so you win one copy each of cards 3 and 4.
        foldl
          ( \cardsWon line@Line {cardNumber} ->
              -- Get the current score.
              let score' :: Integer = score line

                  -- Get the previously won count for the current card.
                  previouslyWon' :: Integer = fromMaybe 0 $ Map.lookup cardNumber cardsWon

                  -- Add 1 for the current card
                  totalCardCount = previouslyWon' + 1
               in foldl
                    (\dict' cardWon -> Map.insertWith (+) cardWon totalCardCount dict')
                    (Map.insertWith (+) cardNumber 1 cardsWon) -- Add in 1 for the current card
                    -- For each card won, you get totalCount copies.
                    (traceShowId [(cardNumber + 1) .. (cardNumber + score')])
          )
          Map.empty
          lines'
  pPrint $ sum $ Map.elems state
