{-# LANGUAGE QuasiQuotes #-}

module Y2024.D7 (run) where

import Control.Exception (throw)
import Data.List (intersperse)
import Debug.Trace (trace)
import Text.Pretty.Simple (pPrint)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

data Line
  = Line
  { testValue :: Integer,
    numbers :: [Parsed]
  }
  deriving (Show)

data Parsed = ParsedInt Integer | AnyOperator
  deriving (Show)

data Processed = ProcessedInt Integer | ProcessedOperator Operator
  deriving (Show)

data Operator = Add | Multiply
  deriving (Show, Enum, Bounded)

-- Get all possible values for an Operator
allOperators :: [Operator]
allOperators = [minBound .. maxBound]

-- Generate all Processed replacements for a single Parsed
replaceParsed :: Parsed -> [Processed]
replaceParsed (ParsedInt n) = [ProcessedInt n]
replaceParsed AnyOperator = map ProcessedOperator allOperators

-- Generate all combinations of Processed from a list of Parsed
generateCombinations :: [Parsed] -> [[Processed]]
generateCombinations [] = [[]]
generateCombinations (p : ps) = do
  replacement <- replaceParsed p
  rest <- generateCombinations ps
  return (replacement : rest)

canEvaluate :: Line -> Bool
canEvaluate line =
  let parsedList :: [Parsed] = numbers line
      allCombinations :: [[Processed]] = generateCombinations parsedList
      match =
        any
          ( \combo ->
              let m = (== testValue line) . evaluateParsed $ combo
               in if m then trace (show combo ++ " " ++ show (testValue line)) m else False
          )
          allCombinations
   in match

evaluateParsed :: [Processed] -> Integer
evaluateParsed (ProcessedInt a : rest) = evalHelper a rest
evaluateParsed _ = throw $ userError "Invalid input"

evalHelper :: Integer -> [Processed] -> Integer
evalHelper acc [] = acc -- Base case: return the accumulated result
evalHelper acc (ProcessedOperator op : ProcessedInt n : rest) =
  let newAcc = applyOperator op acc n
   in evalHelper newAcc rest
evalHelper acc _ = acc -- Handle cases where the list is malformed

-- Apply an operator to two integers
applyOperator :: Operator -> Integer -> Integer -> Integer
applyOperator Add = (+)
applyOperator Multiply = (*)

data State
  = State
  {}
  deriving (Show, Eq, Ord)

parseLine :: String -> Line
parseLine line =
  let regex = [r|([0-9]+): *(.+)|] :: String
      (_, _, _, [testNumber, numbers]) = line =~ regex :: (String, String, String, [String])
   in Line
        { testValue = read testNumber,
          numbers = intersperse AnyOperator $ map (ParsedInt . read) $ words numbers
        }

run :: IO ()
run = do
  content <- readFile "./app/Y2024/7.txt"
  let lines' :: [Line] = map parseLine . lines $ content
  pPrint $ sum $ map testValue $ filter canEvaluate lines'

-- 2014958179 wrong