module Main where
import System.IO
import System.Exit
import System.Environment

import Day00
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import Day99
import Aoc

main :: IO ()
main = do
  day <- getArgs >>= parseDay
  inputFile <- return $ "input/day" ++ day ++ ".txt"
  result <- f day inputFile
  putStrLn . Aoc.prettyResult $ result
  where
    f d =
      case d of
         "00" -> Day00.run
         "01" -> Day01.run
         "02" -> Day02.run
         "03" -> Day03.run
         "04" -> Day04.run
         "05" -> Day05.run
         "06" -> Day06.run
         "07" -> Day07.run
         "08" -> Day08.run
         "09" -> Day09.run
         "10" -> Day10.run
         "11" -> Day11.run
         "12" -> Day12.run
         "13" -> Day13.run
         "14" -> Day14.run
         "15" -> Day15.run
         "16" -> Day16.run
         "17" -> Day17.run
         "18" -> Day18.run
         "19" -> Day19.run
         "20" -> Day20.run
         "21" -> Day21.run
         "22" -> Day22.run
         "23" -> Day23.run
         "24" -> Day24.run
         "25" -> Day25.run
         "99" -> Day99.run
         d -> invalidDay d

parseDay :: [String] -> IO String
parseDay ([c] : _)  = return $ "0" ++ [c]
parseDay ([c1, c2] : _) = return $ [c1, c2]
parseDay _ =
  putStrLn "Error: I need a day as an input (max two chars)"
  >> exitWith (ExitFailure 1)

invalidDay :: String -> String -> IO Result
invalidDay d _ =
  (putStrLn $ "Error: " ++ d ++ " is not a valid day (yet).")
  >> exitWith (ExitFailure 1)
