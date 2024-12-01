module Day99 (run) where
import Aoc
import Data.List

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = fileToSections inputFile

part1 :: [[String]] -> String
part1 = concat . concat

part2 :: [[String]] -> String
part2 = (intercalate "; ") . (fmap (intercalate ", "))
