module Day07 (run) where
import Aoc

run :: FilePath -> IO Result
run inputFile = solve2 f f contents
  where
    contents = return ""
    f _ = "Not done"
