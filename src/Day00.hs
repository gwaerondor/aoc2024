module Day00 (run) where
import Aoc

run :: FilePath -> IO Result
run inputFile = solve2 part1 part2 contents
  where
    contents = fileToCsvM read inputFile
    part1 = sum
    part2 = foldl (*) 1
