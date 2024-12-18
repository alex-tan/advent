module Main (main) where

import Control.Exception (throw)
import Options.Applicative
import Y2015.D1 qualified
import Y2015.D1a qualified
import Y2015.D2 qualified
import Y2015.D2a qualified
import Y2015.D3 qualified
import Y2015.D3a qualified
import Y2018.D10 qualified
import Y2018.D10a qualified
import Y2023.D16 qualified
import Y2023.D16a qualified
import Y2023.D4a qualified
import Y2024.D10 qualified
import Y2024.D10a qualified
import Y2024.D11 qualified
import Y2024.D11a qualified
import Y2024.D12 qualified
import Y2024.D12a qualified
import Y2024.D13 qualified
import Y2024.D13a qualified
import Y2024.D14 qualified
import Y2024.D14a qualified
import Y2024.D15 qualified
import Y2024.D15a qualified
import Y2024.D16 qualified
import Y2024.D16a qualified
import Y2024.D18 qualified
import Y2024.D18a qualified
import Y2024.D6 qualified
import Y2024.D6a qualified
import Y2024.D7 qualified
import Y2024.D7a qualified
import Y2024.D8 qualified
import Y2024.D8a qualified
import Y2024.D9 qualified
import Y2024.D9a qualified

data Options = Options
  { year :: Integer,
    day :: String
  }

options :: Parser Options
options =
  Options
    <$> argument auto (metavar "YEAR")
    <*> argument str (metavar "DAY")

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "Run Advent of Code solution")

main :: IO ()
main =
  run =<< execParser opts

run :: Options -> IO ()
run Options {year, day} =
  case (year, day) of
    (2015, "D1") -> Y2015.D1.run
    (2015, "D1a") -> Y2015.D1a.run
    (2015, "D2") -> Y2015.D2.run
    (2015, "D2a") -> Y2015.D2a.run
    (2015, "D3") -> Y2015.D3.run
    (2015, "D3a") -> Y2015.D3a.run
    (2018, "D10") -> Y2018.D10.run
    (2018, "D10a") -> Y2018.D10a.run
    (2023, "D16") -> Y2023.D16.run
    (2023, "D16a") -> Y2023.D16a.run
    (2023, "D4a") -> Y2023.D4a.run
    (2024, "D6") -> Y2024.D6.run
    (2024, "D6a") -> Y2024.D6a.run
    (2024, "D7") -> Y2024.D7.run
    (2024, "D7a") -> Y2024.D7a.run
    (2024, "D8") -> Y2024.D8.run
    (2024, "D8a") -> Y2024.D8a.run
    (2024, "D9") -> Y2024.D9.run
    (2024, "D9a") -> Y2024.D9a.run
    (2024, "D10") -> Y2024.D10.run
    (2024, "D10a") -> Y2024.D10a.run
    (2024, "D11") -> Y2024.D11.run
    (2024, "D11a") -> Y2024.D11a.run
    (2024, "D12") -> Y2024.D12.run
    (2024, "D12a") -> Y2024.D12a.run
    (2024, "D13") -> Y2024.D13.run
    (2024, "D13a") -> Y2024.D13a.run
    (2024, "D14") -> Y2024.D14.run
    (2024, "D14a") -> Y2024.D14a.run
    (2024, "D15") -> Y2024.D15.run
    (2024, "D15a") -> Y2024.D15a.run
    (2024, "D16") -> Y2024.D16.run
    (2024, "D16a") -> Y2024.D16a.run
    (2024, "D18") -> Y2024.D18.run
    (2024, "D18a") -> Y2024.D18a.run
    _ -> throw $ userError "Not handled in main"
